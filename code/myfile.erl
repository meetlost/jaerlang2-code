%% Functions for manipulating files.

-module(myfile).

-export([read/1]).

read(FileName) ->
    case file:read_file(FileName) of
        {ok, Binary} -> Binary;
        {error, Reason} -> throw(Reason)
    end.
