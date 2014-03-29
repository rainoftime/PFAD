
%% @doc Chapter6: Making a century
-module(ch6).

-export([
         solutions/1,
         improved_solutions/1
        ]).

-type expression() :: [pfad_term()].
-type pfad_term()  :: [factor()].
-type factor()     :: [digit()].
-type digit()      :: integer().

-spec solutions([digit()]) -> [expression()].
solutions(Digits) ->
    lists:filter(fun (E) -> good(val_expr(E)) end, expressions(Digits)).

-spec val_expr(expression()) -> integer().
val_expr(Exp) ->
    lists:sum(lists:map(fun val_term/1, Exp)).

-spec val_term(pfad_term()) -> integer().
val_term(Term) ->
    lists:foldl(fun erlang:'*'/2, 1, lists:map(fun val_fact/1, Term)).

-spec val_fact(factor()) -> integer().
val_fact(Factor) ->
    lists:foldl(fun (D, N) -> 10 * N + D end, 0, Factor).

-spec partitions([term()]) -> [[[term()]]].
partitions([])            -> [[]];
partitions([Head | Tail]) ->
    TailResult = partitions(Tail),
    [[[Head] | Rest] || Rest <- TailResult] ++
    [[[Head | RestHead] | RestTail] || [RestHead | RestTail] <- TailResult].

-spec expressions([digit()]) -> [expression()].
expressions(Digits) ->
    lists:flatmap(fun partitions/1, partitions(Digits)).

-spec good(integer()) -> boolean().
good(100) -> true;
good(_)   -> false.
 
-spec improved_solutions([digit()]) -> [expression()].
improved_solutions(Digits) ->
    C = 100,
    Candidates = lists:foldr(fun (X, Acc) -> expand(C, X, Acc) end, [], Digits),
    [Fst || {Fst, _} <- lists:filter(fun ({_, Snd}) -> good(C, Snd) end, Candidates)].

-type value() :: {integer(), integer(), integer(), integer()}.

%% -spec modify(integer(), value()) -> [value()].
%% modify(X, {K, F, T, E}) ->
%%     [{10 * K, K * X + F, T, E},
%%      {10, X, F * T, E},
%%      {10, X, 1, F * T + E}].

-spec good(integer(), value()) -> boolean().
good(C, {_K, F, T, E}) ->
    (F * T + E =:= C).

-spec ok(integer(), value()) -> boolean().
ok(C, {_K, F, T, E}) ->
    (F * T + E =< C).

-spec expand(integer(), integer(), [{expression(), value()}]) -> [{expression(), value()}].
expand(_, X, [])  -> [{[[[X]]], {10, X, 1, 0}}];
expand(C, X, Evs) ->
    lists:append(
      lists:map(fun ({E, V}) ->
                        lists:filter(fun ({_, Snd}) -> ok(C, Snd) end, glue(X, {E, V}))
                end,
                Evs)).

-spec glue(integer(), {expression(), value()}) -> [{expression(), value()}].
glue(X, {[[Xs | Xss] | Xsss], {K, F, T, E}}) ->
    [
     {[[[X | Xs] | Xss] | Xsss],  {10 * K, K * X + F, T, E}},
     {[[[X], Xs | Xss] | Xsss],   {10, X, F * T, E}},
     {[[[X]], [Xs | Xss] | Xsss], {10, X, 1, F * T + E}}
    ].