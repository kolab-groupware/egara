%% Copyright 2014 Kolab Systems AG (http://www.kolabsys.com)
%%
%% Aaron Seigo (Kolab Systems) <seigo a kolabsys.com>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(egara).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() -> application:start(egara).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:info("Starting Egara ..."),
    lager:info("    Initializing the mnesia-based notification queue ..."),
    egara_notification_store:install([node() | nodes()]),
    egara_notification_store:start(),
    lager:info("    Creating worker job queue ..."),
%%    jobs:add_queue(notification_workers,
%%                   [{regulators, [{rate, [ {modifiers,
%%                                           [{cpu, 15, {max, 0}}]}]}]}]),
    lager:info("    Starting the main supervisor ..."),
    egara_sup:start_link().

stop(_State) ->
    ok.
