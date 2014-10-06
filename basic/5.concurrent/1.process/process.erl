-module(process).
-export([start/0, say_something/2]).

say_something(_What, 0) -> done;
say_something(What, Times) -> 
    io:format("~p~n", [What]),
    say_something(What, Times - 1).

start() ->
    spawn(process, say_something, ["hello", 3]),
    spawn(process, say_something, ["bonjour", 2]).
