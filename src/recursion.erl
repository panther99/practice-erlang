-module(recursion).
-author("Nikola Stojakovic").

-export([
    tail_fac/1,
    duplicate/2,
    tail_duplicate/2,
    reverse/1,
    tail_reverse/1,
    sublist_tail/2,
    zip_tail/2,
    quicksort/1,
    quicksort_short/1
]).

tail_fac(N) -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N - 1, N * Acc).

duplicate(0, _) -> [];
duplicate(N, Item) when N > 0 -> [Item | duplicate(N - 1, Item)].

tail_duplicate(N, Item) ->
    tail_duplicate(N, Item, []).

tail_duplicate(0, _, List) ->
    List;
tail_duplicate(N, Item, List) when N > 0 ->
    tail_duplicate(N - 1, Item, [Item | List]).

reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H].

tail_reverse(List) ->
    tail_reverse(List, []).

tail_reverse([], List) ->
    List;
tail_reverse([H|T], Acc) ->
    tail_reverse(T, [H] ++ Acc).

sublist_tail(List, Num) ->
    tail_reverse(sublist_tail(List, [], Num)).

sublist_tail([], Sublist, _) -> Sublist;
sublist_tail(_, Sublist, 0) -> Sublist;
sublist_tail([H|T], Sublist, Num) when Num > 0 -> sublist_tail(T, [H | Sublist], Num - 1).

zip_tail(First, Second) ->
    tail_reverse(zip_tail([], First, Second)).

zip_tail(Zipped, [], _) -> Zipped;
zip_tail(Zipped, _, []) -> Zipped;
zip_tail(Zipped, [FH|FT], [SH|ST]) -> zip_tail([{ FH, SH } | Zipped], FT, ST).

partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
    if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
       H > Pivot -> partition(Pivot, T, Smaller, [H|Larger])
    end.

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
    {Smaller, Larger} = partition(Pivot, Rest, [], []),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

quicksort_short([Pivot|T]) ->
    quicksort_short([ X || X <- T, X < Pivot])
    ++ [Pivot] ++
    quicksort_short([ X || X <- T, X >= Pivot]);
quicksort_short([]) -> [].
