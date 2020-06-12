%% Types
-module(foo1).

-opaque rich_text() :: [{integer(), atom()}].
-export_type([rich_text/0]).

-export([in/0, out/1]).

-spec in() -> rich_text().

in() ->
    [{1, a}, {2, b}].

-spec out(rich_text()) -> ok.

out(_) ->
    ok.
