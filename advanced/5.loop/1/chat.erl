%%%-------------------------------------------------------------------
%%% @author Bunlong <bunlong.van@gmail.com>
%%% @copyright (C) 2015, Bunlong
%%% @doc
%%%
%%% @end
%%% Created : 04 Jan 2015 by Bunlong <bunlong.van@gmail.com>
%%%-------------------------------------------------------------------
-module(chat).

-behaviour(gen_server).

%% API
-export([start_link/0, register/1, unregister/1, send_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {clients=[]}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%gen_server:cast({chat, 'websocket_example@Makara'}, {register, Pid}).
register(Pid) ->
    gen_server:cast(?SERVER, {register, Pid}).

 %%gen_server:cast({chat, 'websocket_example@Makara'}, {unregister, Pid}).
unregister(Pid) ->
    gen_server:cast(?SERVER, {unregister, Pid}).

%%gen_server:cast({chat, 'websocket_example@Makara'}, {text, Message, Pid}).
send_message(Message, Pid) ->
    gen_server:cast(?SERVER, {text, Message, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({register, Pid}, State=#state{clients=Clients}) ->
    io:format("Got register ~p new list ~p~n", [Pid, [Pid|Clients]]),
    {noreply, State#state{clients=[Pid|Clients]}};
handle_cast({unregister, Pid}, State=#state{clients=Clients}) ->
    {noreply, State#state{clients=Clients -- [Pid]}};
handle_cast({text, Message, Pid}, State=#state{clients=Clients}) ->
    io:format("Got message from ~p ~s~n", [Pid, Message]),
    OtherPids=Clients -- [Pid],
    lists:foreach(fun(OtherPid) ->  OtherPid ! {text, Message, self()}  end, OtherPids),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
