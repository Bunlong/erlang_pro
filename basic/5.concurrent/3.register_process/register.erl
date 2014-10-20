-module(register).
-export([start/0, ping/1, pong/0]).

ping(0) ->
    pong ! finished,
    io:format("ping finished~n", []);
ping(N) ->
    pong ! {ping, self()},
    receive
	pong ->
	    io:format("ping received pong~n", [])
    end,
    ping(N - 1).
pong() ->
    receive
	finished ->
	    io:format("pong finished~n", []);
	{ping, Ping_Pid} ->
	    io:format("pong received ping~n", []),
	    Ping_Pid ! pong,
	    pong()
    end.
start() ->
    register(pong, spawn(register, pong, [])),
    spawn(register, ping, [3]).
