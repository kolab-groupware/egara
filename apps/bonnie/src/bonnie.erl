-module(bonnie).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() -> application:start(bonnie).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    error_logger:info_msg("Starting Bonnie ... ~n"),
    bonnie_sup:start_link().

stop(_State) ->
    ok.
