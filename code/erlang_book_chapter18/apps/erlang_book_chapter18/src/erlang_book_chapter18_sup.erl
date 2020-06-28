%%%-------------------------------------------------------------------
%% @doc erlang_book_chapter18_sup
%% @end
%%%-------------------------------------------------------------------

-module(erlang_book_chapter18_sup).

-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% Supervisor callbacks.
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 10},
	ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
