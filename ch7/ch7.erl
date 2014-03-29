%% @doc Chapter7: Building a tree with minimum height
-module(ch7).

-export([
         mincost_tree/1,
         linear_time_mincost_tree/1,
         trees/1,
         cost/1
        ]).

-type tree() :: {leaf, integer()} | {fork, tree(), tree()}.

-type forest() :: [tree()].

-type tree_with_cost() :: {integer(), tree()}.

-spec cost(tree()) -> integer().
cost({leaf, X})    -> X;
cost({fork, U, V}) -> 1 + max(cost(U), cost(V)).

-spec mincost_tree([integer()]) -> tree().
mincost_tree(Fringes) ->
    pfad_util:min_by(fun cost/1, trees(Fringes)).

-spec trees([integer()]) -> [tree()].
trees([X])      -> [{leaf, X}];
trees([X | Xs]) -> lists:flatmap(prefixes(X), trees(Xs)).

-spec prefixes(integer()) -> fun ((tree()) -> [tree()]).
prefixes(X) ->
    fun (Tree) ->
            [{fork, {leaf, X}, Tree}] ++
            case Tree of
                {leaf, _}    -> [];
                {fork, U, V} -> [{fork, U2, V} || U2 <- (prefixes(X))(U)]
            end
    end.

%% @doc foldr version of trees/1
%% -spec trees([integer()]) -> [tree()].
%% trees(Fringes) ->
%%     [Head | Tail] = lists:reverse(Fringes),
%%     Fun = fun (X, Trees) -> lists:flatmap(prefixes(X), Trees) end,
%%     lists:foldl(Fun, [{leaf, Head}], Tail).

%% @doc forest version of trees/1
%% -spec trees([integer()]) -> [tree()].
%% trees(Fringes) ->
%%     lists:map(fun rollup/1, forests(Fringes)).

%% -spec forests([integer()]) -> [forest()].
%% forests(Fringes) ->
%%     [Head | Tail] = lists:reverse(Fringes),
%%     Fun = fun (X, Forests) -> lists:flatmap(prefixes(X), Forests) end,
%%     lists:foldl(Fun, [[{leaf, Head}]], Tail).

%% -spec prefixes(integer()) -> fun ((forest()) -> [forest()]).
%% prefixes(X) ->
%%     fun (Forest) ->
%%             [begin
%%                  {Front, Rear} = lists:split(K, Forest),
%%                  [{leaf, X}, rollup(Front) | Rear]
%%              end || K <- lists:seq(1, length(Forest))]
%%     end.

-spec rollup(forest()) -> tree().
rollup([Head | Tail]) ->
    lists:foldl(fun (Tree1, Tree2) -> {fork, Tree2, Tree1} end, Head, Tail).

-spec linear_time_mincost_tree([integer()]) -> tree().
linear_time_mincost_tree(Fringes) ->
    [Head | Tail] = lists:reverse(Fringes),
    rollup([Tree || {_, Tree} <- lists:foldl(fun insert/2, [leaf(Head)], Tail)]).

-spec leaf(integer()) -> tree_with_cost().
leaf(X) -> {X, {leaf, X}}.

-spec fork(tree_with_cost(), tree_with_cost()) -> tree_with_cost().
fork({Cost1, Tree1}, {Cost2, Tree2}) ->
    {1 + max(Cost1, Cost2), {fork, Tree1, Tree2}}.

-spec insert(integer(), [tree_with_cost()]) -> [tree_with_cost()].
insert(X, Ts) ->
    [leaf(X) | split(X, Ts)].
                    
-spec split(integer(), [tree_with_cost()]) -> [tree_with_cost()].
split(_, Ts = [_])                                         -> Ts;
split(X, Ts = [{UC, _}, {VC, _} | _]) when X < VC, UC < VC -> Ts;
split(X, [U, V | Ts])                                      -> split(X, [fork(U, V) | Ts]).