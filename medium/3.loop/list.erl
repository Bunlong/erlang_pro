-module(list).
-export([loop/1, put/1, get/1, handle_call/2, start/0, start_link/0]).

put(Item) ->
    test ! {put, Item, self()},
    receive
	Reply ->
	    io:format("Reply is ~p~n", [Reply])
    after 1000 ->
	    io:format("test does not work")
    end.

get(Item) ->
    test ! {get, Item, self()},
    receive
	Reply ->
	    io:format("Reply is ~p~n", [Reply])
    after 1000 ->
	    io:format("test does not work")
    end.

loop(State) ->
    receive
	die ->
	    io:format("Ending loop~n");
	Msg ->
	    New_State = case list:handle_call(Msg, State) of
			    {reply, Reply, New_State_Temp} ->
				Pid = element(3, Msg),
				Pid ! Reply,
				New_State_Temp;
			    {noreply, New_State_Temp} ->
				New_State_Temp
			end,
	    list:loop(New_State)
    end.

handle_call({put, Item, _}, State) ->
    New_State = case lists:member(Item, State) of
		    true ->
			State;
		    false ->
			New_List = [Item|State],
			io:format("~w~n", [New_List]),
			New_List
		end,
    {reply, ok, New_State};
handle_call({get, Item, _}, State) ->
    {New_List, Reply} = case lists:member(Item, State) of
			    true ->
				{lists:delete(Item, State), ok};
			    false ->
				{State, not_found}
			end,
    {reply, Reply, New_List}.

start() ->
    register(supervisor, spawn(fun start_link/0)).

start_link() ->    
    io:format("Start process~n"),
    Pid = spawn(fun() -> loop([]) end),
    register(test, Pid),
    monitor(process, Pid),
    receive
	{'DOWN', _, _, _, _} ->
	    list:start_link();
	die ->
	    Pid ! die;
	Other ->
	    io:format("Supervisor got ~p~n", [Other])
    end.
    
    
    
