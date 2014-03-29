

-module(ch3).

-export([
         invert_p12_1/2,
         invert_p12_2/2,
         invert_p13_1/2,
         invert_p13_2/2,
         invert_p14_1/2,
         invert_p15_1/2
        ]).

-export_type([
              binary_fun/0,
              argument_pair/0
             ]).

-type binary_fun() :: fun ((non_neg_integer(), non_neg_integer()) -> non_neg_integer()).
-type argument_pair() :: {non_neg_integer(), non_neg_integer()}.

%% @doc (N) ^ 2 = O(N^2): N = Z + 1
-spec invert_p12_1(binary_fun(), non_neg_integer()) -> [argument_pair()].
invert_p12_1(Fun, Z) ->
    [{X, Y} || X <- lists:seq(0, Z), Y <- lists:seq(0, Z), Fun(X, Y) =:= Z].

%% @doc (N * (N+1)) / 2 = O(N^2): N = Z + 1
-spec invert_p12_2(binary_fun(), non_neg_integer()) -> [argument_pair()].
invert_p12_2(Fun, Z) ->
    [{X, Y} || X <- lists:seq(0, Z), Y <- lists:seq(0, Z - X), Fun(X, Y) =:= Z].

%% @doc (N) ^ 2 = O(N^2): N = Z + 1
-spec invert_p13_1(binary_fun(), non_neg_integer()) -> [argument_pair()].
invert_p13_1(Fun, Z) ->
    find_p13_1({0, Z}, Fun, Z).

-spec find_p13_1(argument_pair(), binary_fun(), non_neg_integer()) -> [argument_pair()].
find_p13_1({FirstX, LastY}, Fun, Z) ->
    [{X, Y} || X <- lists:seq(FirstX, Z), Y <- lists:seq(0, LastY), Fun(X, Y) =:= Z].

%% @doc O(N): N = Z + 1
-spec invert_p13_2(binary_fun(), non_neg_integer()) -> [argument_pair()].
invert_p13_2(Fun, Z) ->
    find_p13_2({0, Z}, Fun, Z).
 
-spec find_p13_2(argument_pair(), binary_fun(), non_neg_integer()) -> [argument_pair()].
find_p13_2({X, Y}, _, Z) when X > Z; Y < 0 ->
    [];
find_p13_2({X, Y}, Fun, Z) ->
    Result = Fun(X, Y),
    if
        Result  <  Z -> find_p13_2({X + 1, Y}, Fun, Z);
        Result =:= Z -> [{X, Y} | find_p13_2({X + 1, Y - 1}, Fun, Z)];
        Result  >  Z -> find_p13_2({X, Y - 1}, Fun, Z)
    end.

%% @doc [WORST] 2 * log(Z) + M + N, [BEST] 2 * log(Z) + min(M, N)
-spec invert_p14_1(binary_fun(), non_neg_integer()) -> [argument_pair()].
invert_p14_1(Fun, Z) -> 
    M = bsearch(fun (Y) -> Fun(0, Y) end, -1, Z + 1, Z),
    N = bsearch(fun (X) -> Fun(X, 0) end, -1, Z + 1, Z),
    find_p14_1({0, M}, {N, 0}, Fun, Z).

-spec bsearch(Fun, integer(), non_neg_integer(), non_neg_integer()) -> non_neg_integer() when
      Fun :: fun ((integer()) -> non_neg_integer()).
bsearch(_Fun, -1, 0, _Z) -> 0;
bsearch(_Fun, 0, -1, _Z) -> 0;
bsearch(_Fun, Low, High, _) when Low + 1 =:= High -> Low;
bsearch(Fun, Low, High, Z) ->
    M = (Low + High) div 2, 
    case Fun(M) =< Z of
        true  -> bsearch(Fun, M, High, Z);
        false -> bsearch(Fun, Low, M, Z)
    end.

-spec find_p14_1(argument_pair(), argument_pair(), binary_fun(), non_neg_integer()) -> [argument_pair()].
find_p14_1({Top, Left}, {Bottom, Right}, _, _) when Top > Bottom; Left < Right ->
    [];
find_p14_1({X, Y}, Edge, Fun, Z) ->
    Result = Fun(X, Y),
    if
        Result  <  Z -> find_p14_1({X + 1, Y}, Edge, Fun, Z);
        Result =:= Z -> [{X, Y} | find_p14_1({X + 1, Y - 1}, Edge, Fun, Z)];
        Result  >  Z -> find_p14_1({X, Y - 1}, Edge, Fun, Z)
    end.

%% @doc O(M * log(N/M))
-spec invert_p15_1(binary_fun(), non_neg_integer()) -> [argument_pair()].
invert_p15_1(Fun, Z) ->
    M = bsearch(fun (Y) -> Fun(0, Y) end, -1, Z + 1, Z),
    N = bsearch(fun (X) -> Fun(X, 0) end, -1, Z + 1, Z),
    find_p15_1({0, M}, {N, 0}, Fun, Z).

-spec find_p15_1(argument_pair(), argument_pair(), binary_fun(), non_neg_integer()) -> [argument_pair()].
find_p15_1({Top, Left}, {Bottom, Right}, _, _) when Top > Bottom; Left < Right ->
    [];
find_p15_1({Top, Left}, {Bottom, Right}, Fun, Z) when (Left - Right) =< (Bottom - Top) ->
    Q = (Left + Right) div 2,
    P = bsearch(fun (X) -> Fun(X, Q) end, Top - 1, Bottom + 1, Z),
    case Fun(P, Q) =:= Z of
        true  -> [{P, Q} | find_p15_1({Top, Left}, {P - 1, Q + 1}, Fun, Z)];
        false -> find_p15_1({Top, Left}, {P, Q + 1}, Fun, Z)
    end ++ 
    find_p15_1({P + 1, Q - 1}, {Bottom, Right}, Fun, Z);
find_p15_1({Top, Left}, {Bottom, Right}, Fun, Z) ->
    P = (Left + Right) div 2,
    Q = bsearch(fun (Y) -> Fun(P, Y) end, Right - 1, Left + 1, Z),
    find_p15_1({Top, Left}, {P - 1, Q + 1}, Fun, Z) ++
    case Fun(P, Q) =:= Z of
        true  -> [{P, Q} | find_p15_1({P + 1, Q - 1}, {Bottom, Right}, Fun, Z)];
        false -> find_p15_1({P + 1, Q}, {Bottom, Right}, Fun, Z)
    end.
