%% @doc utility functions
-module(pfad_util).

-export([
         count_if/2,
         map_tail/2,
         min_by/2,
         fork/2,
         id/1,
         tails/1
        ]).

-spec map_tail(Fun, [Element]) -> [Result] when
      Fun     :: fun ((Element, Tail) -> Result),
      Element :: term(),
      Result  :: term(),
      Tail    :: [Element].
map_tail(Fun, List) ->
    {Results, _} =
        lists:mapfoldl(
          fun (Element, PrevTail) ->
                  Tail = tl(PrevTail),
                  Result = Fun(Element, Tail),
                  {Result, Tail}
          end,
          List,
          List),
    Results.

-spec count_if(Fun, [term()]) -> non_neg_integer() when
      Fun :: fun ((term()) -> boolean()).
count_if(Fun, List) ->
    lists:foldl(fun (X, Count) -> boolean_to_integer(Fun(X)) + Count end,
                0,
                List).

-spec boolean_to_integer(boolean()) -> 0|1.
boolean_to_integer(true)  -> 1;
boolean_to_integer(false) -> 0.

-spec min_by(Fun, [X]) -> X when
      Fun :: fun ((X) -> Cost::term()),
      X   :: term().
min_by(Fun, [Head | Tail]) ->
    {_, Min} =
        lists:foldl(
          fun (X, {MinCost, MinX}) ->
                  case Fun(X) of
                      Cost when Cost < MinCost -> {Cost, X};
                      _                        -> {MinCost, MinX}
                  end
          end,
          {Fun(Head), Head},
          Tail),
    Min.

-spec id(term()) -> term().
id(X) -> X.

-spec fork({Fun, Fun}, term()) -> {term(), term()} when
      Fun :: fun ((term()) -> term()).
fork({Fun1, Fun2}, X) ->
    {Fun1(X), Fun2(X)}.

-spec tails([term()]) -> [[term()]].
tails([]) -> [];
tails(Xs) -> [Xs | tails(tl(Xs))].