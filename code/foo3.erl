%% Types
-module(foo3).

-export([test/0]).

test() ->
    L = foo1:in(),
    test1(L),
    ok.

-spec test1(foo1:rich_text()) -> ok.

test1(L) ->
    [ Y || {Y, _} <- L ],
    ok.
