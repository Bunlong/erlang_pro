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
-export([start_link/0, send_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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

handle_cast({text, Message, Pid}, State) ->
    OtherPids=pg2:get_members(chat) -- [Pid],
    lists:foreach(fun(OtherPid) -> OtherPid ! {text, Message, self()} end, OtherPids),
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
