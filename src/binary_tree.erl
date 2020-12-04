-module(binary_tree).
-author("Nikola Stojakovic").

-export([
    empty/0,
    insert/3,
    lookup/2,
    has_value/2
]).

empty() -> { node, nil }.

insert(NewKey, NewVal, { node, nil }) -> {node, { NewKey, NewVal, { node, nil }, { node, nil }}};
insert(NewKey, NewVal, { node, { Key, Val, Smaller, Larger }}) ->
    if NewKey > Key  -> { node, { Key, Val, Smaller, insert(NewKey, NewVal, Larger) }};
       NewKey < Key  -> { node, { Key, Val, insert(NewKey, NewVal, Smaller), Larger }};
       NewKey == Key -> { node, { NewKey, Val, Smaller, Larger }}
    end.

lookup(_, { node, nil }) -> nil;
lookup(Key, { node, { Key, Val, _, _ }}) -> {ok, Val};
lookup(Key, { node, { OtherKey, _, Smaller, Larger }}) ->
    if Key > OtherKey -> lookup(Key, Larger);
       Key < OtherKey -> lookup(Key, Smaller)
    end.

has_value(Val, Tree) ->
    try has_value1(Val, Tree) of
        false -> false
    catch
        true -> true
    end.

has_value1(_, { node, nil }) ->
    false;
has_value1(Val, { node, {_, Val, _, _}}) ->
    throw(true);
has_value1(Val, { node, {_, _, Left, Right}}) ->
    has_value1(Val, Left),
    has_value1(Val, Right).
