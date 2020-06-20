%%
%% A dist demo.
%%
-module(a_dist_demo).

-export([start/1, loop/0]).

start(Node) ->
    spawn(Node, fun() -> loop() end).

loop() ->
    receive
    {From, {echo, Any}} ->
        From ! Any,
        loop()
    end.
