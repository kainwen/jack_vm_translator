-module(code_gen).

-export([code_gen/1, print_code/2]).

print_code(Commands, OutFile) ->
    Code = string:join(Commands, "\n"),
    file:write_file(OutFile, list_to_binary(Code)).

code_gen({push, Segment, Offset}) ->
    code_gen_push(Segment, Offset);
code_gen({pop, Segment, Offset}) ->
    code_gen_pop(Segment, Offset);
code_gen({math, Op}) when Op =:= add;
                          Op =:= sub ->
    code_gen_compute_arg2(Op);
code_gen({logic, Op}) when Op =:= 'and';
                           Op =:= 'or' ->
    code_gen_compute_arg2(Op);
code_gen(Command) when Command =:= {math, neg};
                       Command =:= {logic, 'not'} ->
    {_, Op} = Command,
    code_gen_compute_arg1(Op);
code_gen({cmp, Cmp}) ->
    code_gen_cmp(Cmp);
code_gen({def_label, Label}) ->
    code_gen_def_label(Label);
code_gen({goto, Label}) ->
    code_gen_goto(Label);
code_gen({ifgoto, Label}) ->
    code_gen_ifgoto(Label);
code_gen({define, FuncName, Nlocs}) ->
    code_gen_define(FuncName, Nlocs);
code_gen({call, FuncName, Nargs}) ->
    code_gen_call(FuncName, Nargs);
code_gen(return) ->
    code_gen_return().

%% 
code_gen_return() ->
   [
    string:concat("@", integer_to_list(get_register_addr(local))),
    "D = M",
    "@15",
    "M = D //save local into R15, frame"
   ] ++
   [
    "@5",
    "AD = D - A //ret is in this addr",
    "D = M //D contains the return addr",
    "@14",
    "M = D //save return addr into R14"
   ] ++
   pop_arg0() ++
   set_sp() ++
   restore_reg(that) ++
   restore_reg(this) ++
   restore_reg(argument) ++
   restore_reg(local) ++
   goto_ret().

%%
%% 1. push returnaddr label
%% @(return_addr)
%% D = A
%% @0
%% A = M
%% M = D
%% @0
%% M = M + 1
code_gen_call(FuncName, Nargs) ->
    ReturnAddrLabel = gen_return_addr_label(),
    push_label(ReturnAddrLabel) ++
    push_reg(local) ++
    push_reg(argument) ++
    push_reg(this) ++
    push_reg(that) ++
    set_arg(Nargs) ++
    set_local() ++
    goto_function(FuncName) ++
    [string:join(["(", ReturnAddrLabel, ")"], "")].

%%
code_gen_define(FuncName, Nlocs) ->
    Asms = lists:duplicate(Nlocs,
                          [
                           "@0", %push constant 0
                           "A = M",
                           "M = 0",
                           "@0",
                           "M = M + 1"
                          ]),
    [
     string:join(["(", atom_to_list(FuncName), ")"], "") |
     lists:foldl(fun (L, Acc) ->
                   Acc ++ L
                 end,
                 [],
                 Asms)
    ].

%%
code_gen_def_label(Label) ->
    [
     string:join(["(", atom_to_list(Label), ")"], "")
    ].

code_gen_goto(Label) ->
    [
     string:concat("@", atom_to_list(Label)),
     "0;JEQ"
    ].

code_gen_ifgoto(Label) ->
    [
     "@0",
     "AM=M-1",
     "D=M",
     string:concat("@", atom_to_list(Label)),
     "D;JNE"
    ].

%% we need branch
%% @0
%% AM = M - 1 //after this M holds arg2
%% D = M // load arg2 into D
%% @0
%% A = M - 1 //after this M holds arg1
%% D = M - D
%% M = 0     //set result = false
%% @END
%% D;J(JUMP_TAG)
%% @0
%% A = M - 1 //after this M holds arg1
%% M = -1
%% (END)
code_gen_cmp(Cmp) ->
    {END_TAG, JUMP_TAG} = gen_cmp_tag(Cmp),
    [
     "@0",
     "AM = M - 1 //after this M holds arg2",
     "D = M //load arg2 into D",
     "@0",
     "A = M - 1 // after this M holds arg1",
     "D = M - D",
     "M = -1 // set result = false",
     string:concat("@", END_TAG),
     string:concat("D;", JUMP_TAG),
     "@0",
     "A = M - 1 //after this M holds arg1",
     "M = 0",
     string:join(["(", END_TAG, ")"], "")
    ].


%%
%% @0
%% A = M - 1
%% D = M //load arg1 into D
%% M = !D
code_gen_compute_arg1(Op) ->
    [
     "@0",
     "A = M - 1",
     "D = M //load arg1 into D",
     string:concat("M = ", get_compute_expr(Op))
    ].

%%
%% @0
%% M = M - 1 // move sp to final place(just the addr of arg2)
%% A = M
%% D = M     // load arg2 into D
%% @0
%% A = M - 1 // after this, M contains the value of arg1
%% M = D + M | D - M ... // do the compute
code_gen_compute_arg2(Op) ->
    [
     "@0",
     "AM = M - 1 //move sp to final place(just the addr of arg2)",
     "D = M //load arg2 into D",
     "@0",
     "A = M - 1 //after this, M contains the value of arg1",
     string:concat("M = ", get_compute_expr(Op))
    ].


%% direct push
%% @i+Offset
%% D = M
%% @0
%% A = M
%% M = D
%% @0
%% M = M + 1

%% indirect push
%% @i          
%% D = M     //fetch base addr from Registers(Rem[i])
%% @Offset
%% A = D + A //after this, M holds the target addr's content
%% D = M     //load the value into D
%% @0
%% A = M
%% M = D     //write the value into Stack
%% @0
%% M = M + 1 //enlarge the stack
code_gen_push(pointer, Bit) ->
    Idx = case Bit of
              0 ->
                  get_register_addr(this);
              1 ->
                  get_register_addr(that)
          end,
    [
     string:concat("@", integer_to_list(Idx)),
     "D=M",
     "@0",
     "A=M",
     "M=D",
     "@0",
     "M=M+1"
    ];
code_gen_push(Segment, Offset) ->
    Asm1 = fetch_value(Segment, Offset),
    Asm2 = push_D_to_stack(),
    Asm1 ++ Asm2.

%% Direct pop
%% @0
%% M = M - 1
%% D = M //M now is the value want to pop
%% @Addr
%% M = D

%% Indirect pop
%% @Base
%% D = M //save base into D
%% @Offset
%% D = D + A //target addr
%% @R13
%% M = D //save target addr into R13
%% @0
%% M = M - 1
%% A = M //after this M contain the value need save
%% D = M //load value in D
%% @13
%% A = M
%% M = D
code_gen_pop(pointer, Bit) ->
    Idx = case Bit of
              0 ->
                  get_register_addr(this);
              1 ->
                  get_register_addr(that)
          end,
    [
     "@0",
     "AM=M - 1",
     "D=M",
     string:concat("@", integer_to_list(Idx)),
     "M=D"
    ];
code_gen_pop(Segment, Offset) when Segment =:= static ->
    %% pop static 3
    %% 
    ModuleName = env:query(),
    VarName = string:join([ModuleName, integer_to_list(Offset)], "."),
    [
     "@0",
     "AM = M-1",
     "D = M",
     string:concat("@", VarName),
     "M = D"
    ];
code_gen_pop(Segment, Offset) when Segment =:= temp ->
    RegAddr = get_global_addr(Segment),
    TargetAddr = RegAddr + Offset,
    [
     "@0",
     "AM = M - 1",
     "D = M",
     string:concat("@", integer_to_list(TargetAddr)),
     "M = D"
    ];
code_gen_pop(Segment, Offset) ->
    RegAddr = get_register_addr(Segment),
    [
     string:concat("@", integer_to_list(RegAddr)),
     "D = M   //save the base addr into D",
     string:concat("@", integer_to_list(Offset)),
     "D = D + A // D holds the target addr now",
     "@13",
     "M = D //save target into R13",
     "@0",
     "AM = M - 1",
     "D = M   //load value in D",
     "@13",
     "A = M",
     "M = D"
    ].

%% internal helpers

%% load value in target addr to regD
fetch_value(Segment, Offset) when Segment =:= constant ->
    [
     string:concat("@", integer_to_list(Offset)),
     "D = A      //now D holds the constant value"
    ];
fetch_value(Segment, Offset) ->
    Asm1 = computeTargetAddr(Segment, Offset),
    Asm2 = ["D = M     //load the value into D"],
    Asm1 ++ Asm2.

%% compute the target mem addr, load it into regA
computeTargetAddr(Segment, Offset) when Segment =:= local;
                                        Segment =:= argument;
                                        Segment =:= this;
                                        Segment =:= that ->
    RegAddr = get_register_addr(Segment),
    [
     string:concat("@", integer_to_list(RegAddr)),
     "D = M //fetch base addr into D",
     string:concat("@", integer_to_list(Offset)),
     "A = D + A //after this, M holds the target addr's content"
    ];
computeTargetAddr(Segment, Offset) when Segment =:= static ->
    ModuleName = env:query(),
    VarName = string:join([ModuleName, integer_to_list(Offset)], "."),
    [
     string:concat("@", VarName)
    ];
computeTargetAddr(Segment, Offset) when Segment =:= temp ->
    RegAddr = get_global_addr(Segment),
    TargetAddr = RegAddr + Offset,
    [
     string:concat("@", integer_to_list(TargetAddr))
    ].

%% value stored in regD
%% push it into stack
push_D_to_stack() ->
    [
     "@0",                                        
     "A = M",                                 
     "M = D     //write the value into Stack",
     "@0",                                   
     "M = M + 1 //enlarge the stack"
    ].

get_register_addr(Segment) ->
    Plist = [{local, 1}, {argument, 2}, {this, 3}, {that, 4}],
    {_, Idx} = lists:keyfind(Segment, 1, Plist),
    Idx.

get_global_addr(temp) -> 5;
get_global_addr(static) -> 16.

get_compute_expr(Op) ->
    Plist = [
             {add, "D+M"},
             {sub, "M-D"},
             {'and', "D&M"},
             {'or', "D|M"},
             {'neg', "-M"},
             {'not', "!M"}
            ],
    {_, Expr} = lists:keyfind(Op, 1, Plist),
    Expr.

gen_cmp_tag(Cmp) ->
    END_TAG = string:concat("end", random_str()),
    JUMP_TAG = string:concat("J", string:to_upper(atom_to_list(Cmp))),
    {END_TAG, JUMP_TAG}.

random_str() ->
    Src = "abcdefghijklmnopqrstuvwxyz0123456789",
    L = length(Src),
    [lists:nth(rand:uniform(L), Src)
     || _I <- lists:seq(1, 6)].    

gen_return_addr_label() ->
    string:concat("Returnaddr_", random_str()).


push_label(FuncName) ->
    [
     string:concat("@", FuncName),
     "D = A //D is the label value", 
     "@0",
     "A = M",
     "M = D",
     "@0",
     "M = M + 1"
    ].

push_reg(Reg) ->
    Addr = get_register_addr(Reg),
    [
     string:concat("@", integer_to_list(Addr)),
     "D = M",
     "@0",
     "A = M",
     "M = D",
     "@0",
     "M = M + 1"
    ].

set_arg(N) ->
    %% set Reg[Arg] = SP - N - 5
    [
     "@0",
     "D = M",
     string:concat("@", integer_to_list(N)),
     "D = D - A //D now is SP - N",
     "@5",
     "D = D - A //D now is SP - N - 5",
     string:concat("@", integer_to_list(get_register_addr(argument))),
     "M = D"
    ].

set_local() ->
    [
     "@0",
     "D = M",
     string:concat("@", integer_to_list(get_register_addr(local))),
     "M = D"
    ].

pop_arg0() ->
    code_gen({pop, argument, 0}).

set_sp() ->
    [
     string:concat("@", integer_to_list(get_register_addr(argument))),
     "D = M + 1",
     "@0",
     "M = D"
    ].

restore_reg(Reg) ->
    Offset = get_offset_of_reg(Reg),
    [
     "@15",
     "D = M",
     string:concat("@", integer_to_list(Offset)),
     "A = D-A",
     "D = M",
     string:concat("@", integer_to_list(get_register_addr(Reg))),
     "M = D"
    ].

goto_ret() ->
    [
     "@14",
     "A = M",
     "0;JEQ"
    ].

get_offset_of_reg(Reg) ->
    Plist = [{local, 4}, {argument, 3}, {this, 2}, {that, 1}],
    {_, Idx} = lists:keyfind(Reg, 1, Plist),
    Idx.

goto_function(FuncName) ->
    [
     string:concat("@", atom_to_list(FuncName)),
     "0;JEQ"
    ].
