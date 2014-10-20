-module(list).
-export([loop/1, reg_loop/1]).

%% emacs@bunlong> list:reg_loop(test).
%% emacs@bunlong> test ! {self(), 1}.
%% emacs@bunlong> test ! {self(), 2}.
%% emacs@bunlong> flush().

loop(List) ->
    receive
	die ->
	    die;
	{Pid, Num} when is_integer(Num) ->
	    New_List = [Num|List],
	    io:format("Got ~w~n", [New_List]),
	    Pid ! {ok, New_List},
	    list:loop(New_List);
	Other ->
	    io:format("Strange ~p~n", [Other]),
	    list:loop(List)
    end.

reg_loop(Atom) ->
    register(Atom, spawn(fun() -> list:loop([]) end)).
