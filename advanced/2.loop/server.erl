%%%-------------------------------------------------------------------
%%% @author Bunlong <bunlong@bunlong>
%%% @copyright (C) 2014, Bunlong
%%% @doc
%%%
%%% @end
%%% Created : 18 Nov 2014 by Bunlong <bunlong@bunlong>
%%%-------------------------------------------------------------------
-module(server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, put/1, get/1]).

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
    {ok, #state{list=[]}}.

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
handle_call({get, Item}, _From, #state{list=List}) ->
    {Reply, New_List} = case lists:keysearch(Item, 1, List) of
			    {value, {Field, Count}} ->
				case (Count-1) == 0 of
				    true ->
					{ok, lists:keydelete(Field, 1, List)};
				    false ->
					{ok, lists:keystore(Field, 1, List, {Field, Count-1})}
				end;
			    _Other ->
				{not_found, List}
			end,
    io:format("List: ~w~n", [New_List]),
    {reply, Reply, #state{list=New_List}}.

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
handle_cast({put, Item}, #state{list=List}) ->
    Count = case lists:keysearch(Item, 1, List) of
		{value, {_Field, Count_Temp}} ->
		    Count_Temp;
		_Other ->
		    0
		end,
    New_List = lists:keystore(Item, 1, List, {Item, Count+1}),
    io:format("List: ~w~n", [New_List]),
    {noreply, #state{list=New_List}}.

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
put(Item) ->
    gen_server:cast(server, {put, Item}).

get(Item) ->
    gen_server:call(server, {get, Item}).
