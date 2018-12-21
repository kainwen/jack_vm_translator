-module(env).

-export([start/0, loop/1, query/0, set/1]).

start() ->
    case whereis(env) of
        undefined ->
            Pid = spawn(?MODULE, loop, [noname]),
            register(env, Pid),
            ok;
        _ ->
            ok
    end.

query() ->
    env ! {query, self()},
    receive
        Name ->
            Name
    end.

set(Name) ->
    env ! {set, self(), Name},
    receive
        ok ->
            ok
    end.

loop(Name) ->
    receive
        {query, From} ->
            From ! Name,
            loop(Name);
        {set, From, NewName} ->
            From ! ok,
            loop(NewName);
        _ ->
            loop(Name)
    end.
