%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(mochirest).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the mochirest server.
start() ->
    mochirest_deps:ensure(),
    ensure_started(crypto),
    inets:start(),
    ensure_started(ecouch),
    application:start(mochirest).

%% @spec stop() -> ok
%% @doc Stop the mochirest server.
stop() ->
    Res = application:stop(mochirest),
    application:stop(crypto),
    application:stop(ecouch),
    inets:stop(),
    Res.
