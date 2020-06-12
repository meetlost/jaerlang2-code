%% Types

-module(foo).

-export([start/2]).

-spec start(A, B) -> integer() when
    A :: integer(),
    B :: integer().
start(A, B) ->
    A + B.
