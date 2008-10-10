%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the mochirest application.

-module(mochirest_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mochirest.
start(_Type, _StartArgs) ->
    mochirest_deps:ensure(),
    mochirest_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mochirest.
stop(_State) ->
    ok.
