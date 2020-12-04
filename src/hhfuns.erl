-module(hhfuns).

-export([
    one/0,
    two/0,
    add/2,
    increment/1,
    decrement/1,
    map/2,
    incr/1,
    decr/1,
    even/1,
    filter/2,
    is_even/1,
    max/1,
    min/1,
    fold/3,
    max_fold/1,
    min_fold/1,
    map_fold/2,
    filter_fold/2,
    custom_all/2,
    custom_any/2
]).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

even(L) -> lists:reverse(even(L, [])).

even([], Result) -> Result;
even([H|T], Result) when H rem 2 == 0 ->
    even(T, [H | Result]);
even([_|T], Result) ->
    even(T, Result).

is_even(X) -> X rem 2 == 0.

filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).

filter(_, [], Left) -> Left;
filter(Pred, [H|T], Left) ->
    case Pred(H) of
        true  -> filter(Pred, T, [H|Left]);
        false -> filter(Pred, T, Left)
    end.

max([]) -> nil;
max([H|T]) -> max2(T, H).

max2([], Number) -> Number;
max2([H|T], Number) when H > Number -> max2(T, H);
max2([_|T], Number) -> max2(T, Number).

min([H|T]) -> min2(T, H).

min2([], Number) -> Number;
min2([H|T], Number) when H < Number -> min2(T, H);
min2([_|T], Number) -> min2(T, Number).

fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H, Start), T).

custom_all(F, L) -> custom_all(F, L, true).

custom_all(_, [], true) -> true;
custom_all(F, [H|T], true) ->
    case F(H) of
        true -> custom_all(F, T, true);
        false -> false
    end.

custom_any(F, L) -> custom_any(F, L, false).

custom_any(_, [], false) -> false;
custom_any(F, [H|T], false) ->
    case F(H) of
        true  -> true;
        false -> custom_any(F, T, false)
    end.

%% Implement other functions with fold

max_fold([H|T]) ->
    F = fun (A, B) when A > B -> A; (_, B) -> B end,
    fold(F, H, T).

min_fold([H|T]) ->
    F = fun (A, B) when A < B -> A; (_, B) -> B end,
    fold(F, H, T).

map_fold(F, L) ->
    lists:reverse(fold(fun (X, Acc) -> [F(X)|Acc] end, [], L)).

filter_fold(Pred, L) ->
    F = fun (H, Acc) ->
        case Pred(H) of
            true -> [H|Acc];
            false -> Acc
        end
    end,
    lists:reverse(fold(F, [], L)).
