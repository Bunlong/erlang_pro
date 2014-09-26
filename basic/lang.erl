-module(lang).
-export([double/1, max/2, compare/2, month_length/2, area/1, convert/2, convert_length/1, sum_list/1, increase/1, list_length/1, member/2, look_up_key/2, look_up_key_and_update_if_integer/3, list_max/1, reverse/1, duplicate/2, delete/2, append/1, sum_n/1, fac/1, sum/1, mul/1, map/2, filter/2, foreach/2]).

%% lang:double(10).
double(N) -> 2 * N.

%% lang:max(10, 5).
max(X, Y) ->
    if 
	X > Y -> X;
	true -> Y
    end.

%% lang:compare(A, B).
compare(M, N) ->
    if
	M == 5 ->
	    io:format("M == 5~n", []),
	    m_equals_5;
	N == 6 ->
	    io:format("N == 6~n", []),
	    n_equals_6;
	M == 2, N == 3 ->
	    io:format("M == 2, N == 3~n", []),
	    m_equals_2_and_n_equals_3;
	M == 1; N == 7->
	    io:format("M == 1; N == 7~n", []),
	    m_equals_1_or_n_equals_7;
	true ->
	    io:format("nothing~n")
    end.

%% lang:month_length(Month, Year).
month_length(Month, Year) ->
    Leap = if
	       trunc(Year / 400) * 400 == Year -> leap;
	       trunc(Year / 100) * 100 == Year -> not_leap;
	       trunc(Year / 4) * 4 == Year -> leap;
	       true -> not_leap
	   end,
    case Month of 
	sep -> 30;
	apr -> 30;
	jun -> 30;
	nov -> 30;
	feb when Leap == leap -> 29;
	feb -> 28;
	jan -> 31;
	mar -> 31;
	may -> 31;
	jul -> 31;
	aug -> 31;
	oct -> 31;
	dec -> 31
    end.

%% lang:area({rectangle, 300, 100}).
area({rectangle, Width, Height}) -> Width * Height;
area({square, X}) -> X * X;
area({circle, R}) -> 3.14159 * R * R.

%% lang:convert(3, inch).
%% lang:convert(10, centimeter).
convert(M, inch) -> M / 2.54;
convert(N, centimeter) -> N * 2.54.

%% lang:convert_length({inch, 5}).
%% lang:convert_legnth(lang:convert_length({inch, 5})).
convert_length({centimeter, M}) -> {inch, M / 2.54};
convert_length({inch, N}) -> {centimeter, N * 2.54}.

%% lang:sum_list([1, 2, 3, 4, 5]).
sum_list([]) -> 0;
sum_list([H|T]) -> H + sum_list(T).

%% lang:increase([1, 2, 3, 4, 5]).
increase([]) -> [];
increase([H|T]) -> [H + 1|increase(T)].

%% lang:list_length([1, 2, 3, 4, 5]).
list_length([]) -> 0;
list_length([_H|T]) -> 1 + list_length(T).

%% lang:member(a, [a, b, c, d]).
member(_Item, []) -> false;
member(Item, [Item|_T]) -> true;
member(Item, [_H|T]) -> member(Item, T).

%% lang:look_up_key(age, [{name, "Bunlong"}, {age, 30}]).
look_up_key(_Key, []) -> "underfined";
look_up_key(Key, [{Key, Value}|_T]) -> Value;
look_up_key(Key, [_|T]) -> look_up_key(Key, T).

%% lang:look_up_key_and_update_if_integer(age, [{name, "Bunlong"}, {age, 30}], 2).
look_up_key_and_update_if_integer(Key, [H|T], Number) when is_list(Number) -> look_up_key_and_update_if_integer(Key, [H|T], Number);
look_up_key_and_update_if_integer(_Key, [], _Value) -> "undefined";
look_up_key_and_update_if_integer(Key, [{Key, Value}|_T], Number) -> Value + Number;
look_up_key_and_update_if_integer(Key, [_H|T], Number) -> look_up_key_and_update_if_integer(Key, T, Number).

%% lang:list_max([1, 3, 5, 6, 8, 2]).
list_max([H|T]) -> cal_max(T, H).
cal_max([], T) -> T;
cal_max([H|T], Result_so_far) when H > Result_so_far -> cal_max(T, H);
cal_max([_H|T], Result_so_far) -> cal_max(T, Result_so_far).

%% lang:reverse([1, 2, 3, 4, 5]).
reverse(List) -> reverse_list(List, []).
reverse_list([], Reversed_list) -> Reversed_list;
reverse_list([H|T], Reversed_list) -> reverse_list(T, [H|Reversed_list]).

%% lange:duplicate(5, water).
duplicate(0, _Item) -> [];
duplicate(Times, Item) -> [Item|duplicate(Times - 1, Item)].

%% lang:delete(a, [a, b, c]).
delete(_Item, []) -> [];
delete(Item, [H|T]) ->
    case Item =/= H of
	true -> [H|delete(Item, T)];
	false -> delete(Item, T)
    end.

%% lang:append([[1, 2, 3], [a, b], [4, 5, 6]]).
append([]) -> [];
append([H|T]) -> H ++ append(T).

%% lang:sum_n(10).
sum_n(1) -> 1;
sum_n(N) -> N + sum_n(N - 1).

%% lang:fac(10).
fac(1) -> 1;
fac(N) -> N * fac(N - 1).

%% lang:sum(10).
sum(1) -> 1;
sum(N) when N > 1 -> N + sum(N - 1).

%% lang:mul(5).
mul(1) -> 1;
mul(N) when N > 1 -> N * mul(N - 1).

%% lang:map(fun(X) -> X + 3 end, [1, 2, 3]).
map(_F, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].

%% lang:filter(fun(X) -> X rem 2 == 0 end, [1, 2, 3, 4, 5]).
filter(_F, []) -> [];
filter(F, [H|T]) -> 
    case F(H) of
	true -> [H|filter(F, T)];
	false -> filter(F, T)
    end.

%% Print_city = fun({City, {Temp, Value}}) -> io:format("~w ~w ~w~n", [City, Temp, Value]) end.
%% lang:foreach(Print_city, [{Phnom Penh, {t, 10}}, {moscow, {c, -10}}]).
foreach(_F, []) -> ok;
foreach(F, [H|T]) ->
    F(H),
    foreach(F, T).
