-module(timeout).

-export([start_ping/1, start_pong/0,  ping/2, pong/0]).

ping(0, _Pong_Node) ->
    io:format("ping finished~n", []);
ping(N, Pong_Node) ->
    {pong, Pong_Node} ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_Node).

pong() ->
    receive
        {ping, Ping_Pid} ->
            io:format("Pong received ping~n", []),
            Ping_Pid ! pong,
            pong()
    after 5000 ->
            io:format("Pong timed out~n", [])
    end.

start_pong() ->
    register(pong, spawn(tut19, pong, [])).

start_ping(Pong_Node) ->
    spawn(tut19, ping, [3, Pong_Node]).

%% On (pong@kosken):

%%    (pong@kosken)1> tut19:start_pong().
%%    true
%%    Pong received ping
%%    Pong received ping
%%    Pong received ping
%%    Pong timed out


%% On (ping@gollum):

%%    (ping@gollum)1> tut19:start_ping(pong@kosken).
%%    <0.36.0>
%%    Ping received pong
%%    Ping received pong
%%    Ping received pong
%%    ping finished   
