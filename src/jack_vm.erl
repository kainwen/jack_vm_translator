-module(jack_vm).

-export([scan_and_trans/2, scan_and_compile/1, scan_and_trans_dir/2]).

scan_and_trans(InFile, OutFile) ->
    ok = env:start(),
    ModuleName = get_module_name(InFile),
    ok = env:set(ModuleName),
    Cmds = jack_vm_parser:scan_and_parse(InFile),
    Asm = lists:foldl(fun(X, Acc) ->
                              Acc ++ X
                      end,
                      [],
                      [code_gen:code_gen(Cmd) || Cmd <- Cmds]),
    code_gen:print_code(Asm, OutFile).

scan_and_compile(InFile) ->
    ok = env:start(),
    ModuleName = get_module_name(InFile),
    ok = env:set(ModuleName),
    Cmds = jack_vm_parser:scan_and_parse(InFile),
    lists:foldl(fun(X, Acc) ->
                        Acc ++ X
                end,
                [],
                [code_gen:code_gen(Cmd) || Cmd <- Cmds]).

scan_and_trans_dir(Dirname, OutFile) ->
    {ok, AllFiles} = file:list_dir(Dirname),
    VmFiles = [Fn || Fn <- AllFiles, filename:extension(Fn) =:= ".vm"],
    Codes = lists:foldl(fun (X, Acc) ->
                                Acc ++ X
                        end,
                        [],
                        [scan_and_compile(filename:join(Dirname, Fn))
                         || Fn <- VmFiles]),
    InitSp = [
              "@256",
              "D=A",
              "@0",
              "M=D"
    ],
    Call_sysinit = code_gen:code_gen({call, 'Sys.init', 0}),
    FinalCode = InitSp ++ Call_sysinit ++ Codes,
    code_gen:print_code(FinalCode, OutFile).

get_module_name(InFile) ->
    Bn1 = filename:basename(InFile),
    filename:basename(Bn1, ".vm").
