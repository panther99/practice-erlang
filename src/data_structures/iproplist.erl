-module(iproplist).
-author("Nikola Stojaković").

-export([
   build_from_tuple/1,
   build_from_list/1,
   lookup/2,
   nth/2,
   update/3,
   keys/1,
   values/1,
   sort_by_keys/1,
   sort_by_values/1
]).

build([], PropList) -> PropList;
build([Single], PropList) ->
    build([], [{Single, undefined} | PropList]);
build([First, Second | T], PropList) ->
    build(T, [{First, Second} | PropList]).

%% @doc Builds a proplist from a tuple.

build_from_tuple(Tuple) when is_tuple(Tuple) ->
    lists:reverse(build(tuple_to_list(Tuple), [])).

%% @doc Builds a proplist from a list.

build_from_list(List) when is_list(List) ->
    lists:reverse(build(List, [])).

%% @doc Looks for a tuple with the given key in a proplist.

lookup(Key, [H|T]) ->
    lookup(Key, H, T).

lookup(Key, {Key, Value}, _) -> {Key, Value};
lookup(Key, {_, _}, [H|T]) ->
    lookup(Key, H, T);
lookup(_, _, []) -> undefined.

%% @doc Returns nth element of a proplist (if it exists).

nth(N, PropList) ->
    lists:nth(N, PropList).

%% @doc Returns a new proplist with updated value at the given key.

update(Key, Value, [H|T]) ->
    update(Key, Value, H, T, 1).

update(Key, Value, {Key, _}, _, Acc) ->
    {Acc, {Key, Value}};
update(Key, Value, _, [H|T], Acc) ->
    update(Key, Value, H, T, Acc);
update(Key, Value, _, [], _) ->
    {not_found, {Key, Value}}.

%% @doc Returns a list of the keys in a proplist.

keys(PropList) ->
    lists:reverse(keys(PropList, [])).

keys([], Keys) -> Keys;
keys([{Key, _}|T], Keys) ->
    keys(T, [Key|Keys]).

%% @doc Returns a list of the values in a proplist.

values(PropList) ->
    lists:reverse(values(PropList, [])).

values([], Values) -> Values;
values([{_, Value}|T], Values) ->
    values(T, [Value|Values]).

%% @docs Returns a new proplist with elements sorted by the keys.

sort_by_keys(PropList) ->
    F = fun ({KeyA, ValueA}, {KeyB, ValueB}) -> {KeyA, ValueA} =< {KeyB, ValueB} end,
    lists:sort(F, PropList).

%% @docs Returns a new proplist with elements sorted by the values.

sort_by_values(PropList) ->
    F = fun ({KeyA, ValueA}, {KeyB, ValueB}) -> {ValueA, KeyA} =< {ValueB, KeyB} end,
    lists:sort(F, PropList).
