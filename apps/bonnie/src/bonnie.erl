-module(bonnie).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() -> application:start(bonnie).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %%TODO:
    %%  - set the mnesia storage path
    %%  - install the mnesia schema
    error_logger:info_msg("Starting Bonnie ... ~n"),
    application:set_env(mnesia, dir, "db"),
    notification_store:install([node() | nodes()]),
    notification_store:start(),
    bonnie_sup:start_link().

stop(_State) ->
    ok.
