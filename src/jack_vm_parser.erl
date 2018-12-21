-module(jack_vm_parser).

-export([scan_and_parse/1]).


%% Export APIs
scan_and_parse(VmFile) ->
    {ok, Data} = file:read_file(VmFile),
    Code = string:concat(binary_to_list(Data), "\n"),
    {ok, Toks, _} = jack_vm_tok:string(Code),
    Tokens = [T || T <- Toks, T /= {comment}],
    parse(Tokens).

%% Internal functions
parse(Toks) ->
    parse(Toks, []).

parse([], Acc) ->
    lists:reverse(Acc);
parse(Toks, Acc) ->
    {Command, RemToks} = parse_single_command(Toks),
    parse(RemToks, [Command|Acc]).

%% parse single command dispatch
parse_single_command(Toks=[push|_]) ->
    parse_push(Toks);
parse_single_command(Toks=[pop|_]) ->
    parse_pop(Toks);
parse_single_command(Toks=[{math, _}|_]) ->
    parse_math(Toks);
parse_single_command(Toks=[{cmp, _}|_]) ->
    parse_cmp(Toks);
parse_single_command(Toks=[{logic, _}|_]) ->
    parse_logic(Toks);
parse_single_command([{comment}|RemToks]) ->
    parse_single_command(RemToks);
parse_single_command(Toks=[def_label|_]) ->
    parse_label_def(Toks);
parse_single_command(Toks=[goto|_]) ->
    parse_goto(Toks);
parse_single_command(Toks=[ifgoto|_]) ->
    parse_ifgoto(Toks);
parse_single_command(Toks=[define|_]) ->
    parse_function_def(Toks);
parse_single_command(Toks=[call|_]) ->
    parse_call(Toks);
parse_single_command(Toks=[return|_]) ->
    parse_return(Toks).


%% sub handlers
parse_push([push, {segment, Segname}, Number|RemToks]) ->
    {{push, Segname, Number}, RemToks}.

parse_pop([pop, {segment, Segname}, Number|RemToks]) ->
    {{pop, Segname, Number}, RemToks}.

parse_math([{math, Operator}|RemToks]) ->
    {{math, Operator}, RemToks}.

parse_cmp([{cmp, Operator}|RemToks]) ->
    {{cmp, Operator}, RemToks}.

parse_logic([{logic, Operator}|RemToks]) ->
    {{logic, Operator}, RemToks}.

parse_label_def([def_label, {label, Label}|RemToks]) ->
    {{def_label, Label}, RemToks}.

parse_goto([goto, {label, Label}|RemToks]) ->
    {{goto, Label}, RemToks}.

parse_ifgoto([ifgoto, {label, Label}|RemToks]) ->
    {{ifgoto, Label}, RemToks}.

parse_function_def([define, {label, FuncName}, Nlocs|RemToks]) ->
    {{define, FuncName, Nlocs}, RemToks}.

parse_call([call, {label, FuncName}, Nargs|RemToks]) ->
    {{call, FuncName, Nargs}, RemToks}.

parse_return([return|RemToks]) ->
    {return, RemToks}.
