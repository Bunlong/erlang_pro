-module(message).
-export([start/0, ping/2, pong/0]).

ping(0, Pong_Pid) ->
    Pong_Pid ! finished,
    io:format("ping finished!n", []);
ping(N, Pong_Pid) ->
    Pong_Pid ! {ping, self()},
    receive
	pong ->
	    io:format("ping received pong~n", [])
    end,
    ping(N - 1, Pong_Pid).

pong() ->
    receive
	finished ->
	    io:format("pong finished~n", []);
	{ping, Ping_Pid} ->
	    io:format("pong received ping~n", []),
	    Ping_Pid ! pong
    end.

start() ->
    Pong_Pid = spawn(message, pong, []),
    spawn(message, ping, [3, Pong_Pid]).
