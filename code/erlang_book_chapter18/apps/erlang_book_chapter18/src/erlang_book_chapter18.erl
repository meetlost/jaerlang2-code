%%%-------------------------------------------------------------------
%% @doc erlang_book_chapter18
%% @end
%%%-------------------------------------------------------------------

-module(erlang_book_chapter18).

-behaviour(application).

-include("erlang_book_chapter18.hrl").

-export([start/2, stop/1]).

start(_Application, _Type) ->
	% Start cowboy.
	Dispatch = cowboy_router:compile([
									  {'_',
									   [
										{"/", cowboy_static, {priv_file, erlang_book_chapter18, "static/index.html"}},
										{"/websocket/:app", erlang_book_chapter18_ws_h, []},
										{"/[...]", cowboy_static, {priv_dir, erlang_book_chapter18, "static"}}
									   ]}
									 ]),
	{ok, ConfigPort} = application:get_env(?APP_NAME, port),
	{ok, _} = cowboy:start_clear(http,
								 [
								  {port, ConfigPort}
								 ],
								 #{env => #{dispatch => Dispatch}}),

	erlang_book_chapter18_sup:start_link().

stop(_Application) ->
    ok = cowboy:stop_listener(http).
