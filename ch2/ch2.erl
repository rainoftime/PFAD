

%% @doc Chapter 2: A surpassing problem
-module(ch2).

-export([
         msc/1,
         devide_and_conquer_msc/1
        ]).

%% @doc O(N ^ 2): N = length(List)
-spec msc([Element]) -> MaximumSurpasserCount::non_neg_integer() when
      Element :: term().
msc([_|_] = List) ->
    lists:max(pfad_util:map_tail(fun count_surpassers/2, List)).

%% @doc O(N * log(N)): N = length(List)
-spec devide_and_conquer_msc([Element]) -> MaximumSurpasserCount::non_neg_integer() when
      Element :: term().
devide_and_conquer_msc([_|_] = List) ->
    lists:max([Count || {_, Count} <- table(List)]).

-spec table([Element]) -> [{Element, SupasserCount::non_neg_integer()}] when
      Element :: term().
table([X])  -> [{X, 0}];
table(List) -> 
    Size = length(List),
    Half = Size div 2,
    {Left, Right} = lists:split(Half, List),
    join(Size - Half, table(Left), table(Right)).

-spec join(RightSize::non_neg_integer(), Table, Table) -> Table when
      Table :: [{Element::term(), SupasserCount::non_neg_integer()}].
join(_, Left, [])            -> Left;  
join(_, [], Right)           -> Right;
join(RightSize, Left, Right) ->
    [{LeftElement, LeftCount}   | LeftTail]  = Left,
    [{RightElement, RightCount} | RightTail] = Right,
    case LeftElement < RightElement of
        true  -> [{LeftElement, LeftCount + RightSize} | join(RightSize, LeftTail, Right)];
        false -> [{RightElement, RightCount}           | join(RightSize - 1, Left, RightTail)]
    end.

-spec count_surpassers(Element, [Element]) -> non_neg_integer() when
      Element :: term().
count_surpassers(Element, List) ->
    pfad_util:count_if(fun (X) -> X > Element end, List).