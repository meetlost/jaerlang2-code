%%%-------------------------------------------------------------------
%% @doc erlang_book_chapter18_ws_h
%% @end
%%%-------------------------------------------------------------------

-module(erlang_book_chapter18_ws_h).

%% Cowboy callbacks.
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, State) ->
	App = cowboy_req:binding(app, Req),
	App1 = list_to_atom(binary_to_list(App)),
	State1 = [{app, App1}|State],

	% Idle timeout: 5 minutes.
	{cowboy_websocket, Req, State1, #{idle_timeout => 5 * 60 * 1000}}.

websocket_init(State) ->
	{app, App} = lists:keyfind(app, 1, State),
	case App of
		chat2 ->
			case whereis(irc) of
				undefined -> irc:start();
				_Any -> ok
			end;
		_Any -> ok
	end,
	AppPid = spawn_link(App, start, [self()]),
	State1 = [{app_pid, AppPid}|State],
	{[], State1}.

websocket_handle({text, Msg}, State) ->
	{app_pid, AppPid} = lists:keyfind(app_pid, 1, State),
	io:format("~p~n", [Msg]),
	Msg1 = maps:from_list(jsx:decode(Msg)),
	io:format("~p~n", [Msg1]),
	AppPid ! {self(), Msg1},
	{[], State};
websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	{[{text, Msg}], State};
websocket_info(#{cmd := _CMD} = Info, State) ->
	Info1 = jsx:encode(Info),
	Msg = Info1,
	io:format("~p~n", [Msg]),
	{[{text, Msg}], State};
websocket_info(_Info, State) ->
	{[], State}.

terminate(_Reason, _PartialReq, _State) ->
	ok.
