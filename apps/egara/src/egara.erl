-module(egara).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() -> application:start(egara).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    error_logger:info_msg("Starting Egara ... ~n"),
    application:set_env(mnesia, dir, "db"),
    egara_notification_store:install([node() | nodes()]),
    egara_notification_store:start(),
    egara_sup:start_link().

stop(_State) ->
    ok.
