-module(rpnc).
-author("Nikola Stojaković").

-export([
   calc/1
]).

is_operator(Op) ->
    case Op of
        O when O =:= plus;
               O =:= minus;
               O =:= times;
               O =:= divby -> true;
        _                  -> false
    end.

calc_expr(Op, Num1, Num2) ->
    if Op == plus  -> Num1 + Num2;
       Op == minus -> Num1 - Num2;
       Op == times -> Num1 * Num2;
       Op == divby -> Num1 div Num2
    end.

calc(List) ->
    try
        R = safe_calc(List),
        {ok, R}
    catch
        error -> {err, "Invalid input"}
    end.

safe_calc(List) -> calc(List, []).

calc([], [Num]) when is_integer(Num) ->
    Num;

calc([], [Op, Num1, Num2]) ->
    calc_expr(Op, Num2, Num1);

calc([H | T], [SH | ST]) when is_integer(H) and is_integer(SH) ->
    calc(T, [H, SH | ST]);

calc([H | T], [Num1, Num2 | ST]) ->
    case is_operator(H) of
        true  -> calc(T, [calc_expr(H, Num2, Num1) | ST]);
        false -> calc(T, [H, Num1, Num2 | ST])
    end;

calc([H | T], []) ->
    calc(T, [H]).
