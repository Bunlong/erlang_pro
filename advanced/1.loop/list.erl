-module(list).
-export([put/1, get/1, loop/1, start/0, start_link/0]).

put(Item) ->
    test ! {put, Item, self()},
    receive
	Reply ->
	    io:format("reply is ~p~n", [Reply])
    after 100 ->
	    io:format("test does not work")
    end.

get(Item) ->
    test ! {get, Item, self()},
    receive
	Reply ->
	    io:format("reply is ~p~n", [Reply])
    after 1000 ->
	    io:format("test does not work.")
    end.

loop(State) ->
    receive
	die ->
	    io:format("Ending loop.");
	Msg ->
	    New_State = case handle_call(Msg, State) of
			    {reply, Reply, New_State_Temp} ->
				Pid = element(3, Msg),
				Pid ! Reply,
				New_State_Temp;
			    {noreply, New_State_Temp} ->
				New_State_Temp
			end,
	    loop(New_State)
	    
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
    io:format("start process~n"),
    Pid = spawn(fun() -> loop([]) end),
    register(test, Pid),
    monitor(process, Pid),
    receive
	{'DOWN', _, _, _, _} -> start_link();
	die ->
	    Pid ! die;
	Other ->
	    io:format("supervisor got ~p~n", [Other])
    end.

