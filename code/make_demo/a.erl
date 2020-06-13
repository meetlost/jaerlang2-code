-module(a).

-export([start/0]).

-export([test/0]).

start() ->
    "start".

test() ->
    "start" = start(),
    ok.
