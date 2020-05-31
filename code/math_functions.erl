%% Some math functions.

-module(math_functions).

-export([even/1, odd/1, filter/2, split/1, split1/1]).

even(X) when X rem 2 =:= 0 -> true;
even(_) -> false.

odd(X) when X rem 2 =:= 1 -> true;
odd(_) -> false.

filter(F, L) ->
    [ X || X <- L, F(X) =:= true ].

split(L) ->
    {filter(fun even/1, L), filter(fun odd/1, L)}.

split1(L) ->
    split1(L, [], []).

split1([H|T], Evens, Odds) ->
    case H rem 2 of
        0 -> split1(T, [H|Evens], Odds);
        1 -> split1(T, Evens, [H|Odds])
    end;
split1([], Evens, Odds) ->
    {lists:reverse(Evens), lists:reverse(Odds)}.
