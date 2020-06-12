%% Types
-module(foo2).

-export([test/0]).

test() ->
    X = foo1:in(),
    foo1:out(X).
