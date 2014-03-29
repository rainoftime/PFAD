
%% @doc Chapter 5: Sorting pairwize sums
-module(ch5).

-export([
         sortsums/2,
         lambert_sortsums/2
        ]).

%% @doc O(N^2 * log(N)): N = length(Xs) = length(Ys)
-spec sortsums([term()], [term()]) -> [term()].
sortsums(Xs, Ys) ->
   lists:sort([sum(X, Y) || X <- Xs, Y <- Ys]).

-spec lambert_sortsums([term()], [term()]) -> [term()].
lambert_sortsums(Xs, Ys) ->
    [V || {V, _} <- sortsubs(Xs, lists:map(fun negate/1, Ys))].

-spec sortsubs([term()]) -> [{term(), {integer(), integer()}}].
sortsubs([])  -> [];
sortsubs([W]) -> [{W - W, {0, 0}}];
sortsubs(Ws)  ->
    M = length(Ws) div 2,
    {Xs, Ys} = lists:split(M, Ws),
    Xxs = sortsubs(Xs),
    Xys = sortsubs(Xs, Ys),
    Yxs = lists:map(fun ({X, {I, J}}) -> {-X, {J, I}} end, lists:reverse(Xys)),
    Yys = sortsubs(Ys),

    Incl = fun ({X, {I, J}}) -> {X, {M + I, J}} end,
    Incr = fun ({X, {I, J}}) -> {X, {I, M + J}} end,
    Incb = fun ({X, {I, J}}) -> {X, {M + I, M + J}} end,
    
    lists:foldr(fun lists:merge/2, [], [Xxs, lists:map(Incl, Xys), lists:map(Incr, Yxs), lists:map(Incb, Yys)]).

-spec sortsubs([term()], [term()]) -> [{term(), {integer(), integer()}}].
sortsubs(Xs, Ys) ->
    Table0 = table(Xs, Ys),
    XsLen = length(Xs),
    YsLen = length(Ys),
    ToIndex = fun ({A, B, C}) -> A * XsLen * YsLen + B * YsLen + C end,
    Table = list_to_tuple([Rank || {_, Rank} <- lists:sort(lists:zip(lists:map(ToIndex, Table0), lists:seq(1,length(Table0))))]),
    GetRank = fun ({A, B, C}) -> element(ToIndex({A, B, C}) + 1, Table) end,
    lists:sort(fun ({_X, {I, J}}, {_Y, {K, L}}) -> GetRank({0, I, J}) < GetRank({1, K, L}) end,
               subs(Xs, Ys)).

-spec subs([term()], [term()]) -> [{term(), {integer(), integer()}}].
subs(Xs, Ys) ->
    [{sub(X, Y), {I, J}} || {X, I} <- lists:zip(Xs, lists:seq(0, length(Xs) - 1)),
                            {Y, J} <- lists:zip(Ys, lists:seq(0, length(Ys) - 1))].

-spec table([term()], [term()]) -> [{integer(), integer(), integer()}].
table(Xs, Ys) ->
    Xxs = sortsubs(Xs),
    Yys = sortsubs(Ys),
    Tag = fun (I) -> fun ({X, {J, K}}) -> {X, {I, J, K}} end end,
    [Index || {_, Index} <- lists:merge(lists:map(Tag(0), Xxs), lists:map(Tag(1), Yys))].

-spec sum(number(), number()) -> number().
sum(X, Y) -> X + Y.

-spec sub(number(), number()) -> number().
sub(X, Y) -> X - Y.

-spec negate(number()) -> number().
negate(X) -> -X.