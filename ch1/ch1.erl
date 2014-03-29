-module(ch1).

-export([
         minfree/1,
         devide_and_conquer_minfree/1
        ]).

%% @doc O(N^2): N=length(Numbers)
-spec minfree([non_neg_integer()]) -> non_neg_integer().
minfree(Numbers) ->
    hd(lists:dropwhile(fun (X) -> lists:member(X, Numbers) end, lists:seq(0, length(Numbers)))).


%% @doc O(N): N=length(Numbers)
-spec devide_and_conquer_minfree([non_neg_integer()]) -> non_neg_integer().
devide_and_conquer_minfree([])      -> 0;
devide_and_conquer_minfree(Numbers) ->
    Half  = (length(Numbers) + 1) div 2,
    {Left, Right} = lists:partition(fun (X) -> X < Half end, Numbers),
    case length(Left) < Half of
        true  -> devide_and_conquer_minfree(Left);
        false -> Half + devide_and_conquer_minfree([X - Half || X <- Right])
    end.