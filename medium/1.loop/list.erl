-module(list).
-export([add/1, reg_add/1]).

%% emacs@bunlong> list:reg_loop(test).
%% emacs@bunlong> test ! {self(), 1}.
%% emacs@bunlong> test ! {self(), 2}.
%% emacs@bunlong> flush().

add(List) ->
    receive
	die ->
	    die;
	{Pid, Num} when is_integer(Num) ->
	    New_List = [Num|List],
	    io:format("Got ~w~n", [New_List]),
	    Pid ! {ok, New_List},
	    list:add(New_List);
	Other ->
	    io:format("Strange ~p~n", [Other]),
	    list:add(List)
    end.

reg_add(Atom) ->
    register(Atom, spawn(fun() -> list:add([]) end)).
