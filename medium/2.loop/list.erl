-module(list).
-export([loop/1, start/0, put/1, get/1]).

%% emacs@bunlong> list:start().
%% emacs@bunlong> list:put(1).
%% emacs@bunlong> list:put(2).
%% emacs@bunlong> list:put(3).
%% emacs@bunlong> list:get(3).

put(Atom) ->
    test ! {put, Atom, self()},
    receive
	Reply ->
	    io:format("Reply is ~p~n", [Reply])
    after 1000 ->
	    io:format("test not runing")
    end.

get(Atom) ->
    test ! {get, Atom, self()},
    receive
	Reply ->
	    io:format("Reply is ~p~n", [Reply])
    after 1000 ->
	    io:format("test not runing")
    end.

loop(List) ->
    receive
	die ->
	    die;
	{put, Atom, Pid} ->
	    New_List = case lists:member(Atom, List) of
			   true ->
			       Pid ! false,
			       List;
			   false ->
			       New_List_Temp = [Atom|List],
			       io:format("~w~n", [New_List_Temp]),
			       Pid ! ok,
			       New_List_Temp
		       end,
	    loop(New_List);
	{get, Atom, Pid} ->
	    New_List = case lists:member(Atom, List) of
			   true ->
			       Pid ! true,
			       lists:delete(Atom, List);
			   false ->
			       Pid ! false,
			       List
		       end,
	    loop(New_List)
    end.

start() ->
    register(test, spawn(fun() -> loop([]) end)).
