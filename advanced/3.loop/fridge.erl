%%%-------------------------------------------------------------------
%%% @author Bunlong <bunlong@bunlong>
%%% @copyright (C) 2014, Bunlong
%%% @doc
%%%
%%% @end
%%% Created :  4 Jun 2014 by Bunlong <bunlong@bunlong>
%%%-------------------------------------------------------------------
-module(fridge).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {list}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{list = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, Item}, _From, #state{list = List}) ->
    {Reply, New_list} = case lists:keysearch(Item, 1, List) of
			    {value, {Field, Count}} ->
				case (Count - 1) == 0 of
				    true ->
					gen_server:cast({log, log@bunlong}, {log, f("there is no ~p~n",[Field])}),
					{ok, lists:keydelete(Field, 1, List)};
				    false ->
					gen_server:cast({log, log@bunlong}, {log, f("~p taken~n", [Field])}),
					New_list_inner = lists:keystore(Field, 1, List, {Field, Count - 1}),
					gen_server:cast({backup, backup@bunlong}, {backup, New_list_inner}),
					{ok, New_list_inner}
				end;
			    _Other ->
				{not_found, List}
			end,
    {reply, Reply, #state{list = New_list}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({put, Item}, #state{list = List}) ->
    Count = case lists:keysearch(Item, 1, List) of 
		{value, {_Field, Count_inner}} ->
		    Count_inner;
		_Other ->
		    0
	    end,
    New_list = lists:keystore(Item, 1, List, {Item, Count + 1}),
    gen_server:cast({backup, backup@bunlong}, {backup, New_list}),
    gen_server:cast({log, log@bunlong}, {log, f("~p added~n", [Item])}),
    io:format("~p~n", [New_list]),
    {noreply, #state{list = New_list}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
f(Str, Param) ->
    lists:flatten(io_lib:format(Str, Param)).
