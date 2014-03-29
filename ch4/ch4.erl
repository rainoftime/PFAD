


%% @doc Chapter 4: A selection problem
-module(ch4).

-export([
         smallest/3,
         devide_and_conquer_smallest/3
        ]).

%% @doc O(|Xs| + |Ys|)
-spec smallest(non_neg_integer(), ordsets:ordset(term()), ordsets:ordset(term())) -> term().
smallest(Nth, Xs, Ys) -> 
    lists:nth(Nth + 1, ordsets:union(Xs, Ys)).

%% @doc O(log(|Xs| + |Ys|))
-spec devide_and_conquer_smallest(non_neg_integer(), tuple(), tuple()) -> term().
devide_and_conquer_smallest(Nth, Xs, Ys) ->
    search(Nth, Xs, 0, size(Xs), Ys, 0, size(Ys)).

-spec search(non_neg_integer(), tuple(), non_neg_integer(), non_neg_integer(), tuple(), non_neg_integer(), non_neg_integer()) -> term().
search(Nth, _, Xl, Xr, Ys, _, _) when Xl =:= Xr -> at(Nth - Xl, Ys);
search(Nth, Xs, _, _, _, Yl, Yr) when Yl =:= Yr -> at(Nth - Yl, Xs); 
search(Nth, Xs, Xl, Xr, Ys, Yl, Yr) ->
    Xm = (Xl + Xr) div 2,
    Ym = (Yl + Yr) div 2,
    case {at(Xm, Xs) < at(Ym, Ys), Nth =< Xm + Ym} of
        {false, _} -> search(Nth, Ys, Yl, Yr, Xs, Xl, Xr);
        {_, true}  -> search(Nth, Xs, Xl, Xr, Ys, Yl, Ym);
        {_, false} -> search(Nth, Xs, Xm + 1, Xr, Ys, Yl, Yr)
    end.

-spec at(non_neg_integer(), tuple()) -> term().
at(Nth, Tuple) -> element(Nth + 1, Tuple).
