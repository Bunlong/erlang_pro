-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    chat:register(self()),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    chat:send_message(Msg, self()),
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({text, Msg, _Pid}, Req, State) ->
    io:format("~p sending ~s to browser~n", [self(), Msg]),
    {reply, {text, Msg}, Req, State};
websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(Info, Req, State) ->
    io:format("Strange message ~p~n", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    io:format("Unregister"),
    chat:unregister(self()),
    ok.
