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

-module(egara_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Module, Type), { Module, { Module, start_link, []}, permanent, 5000, Type, [Module] }).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    lager:info("    Creating resource pools ..."),
    WorkerSize = application:get_env(egara, worker_pool_size, erlang:system_info(schedulers_online) * 2),
    Pools = [
             { egara_notification_workers, egara_worker, [ { size, WorkerSize }, { max_overflow, 0 }], [ ] },
             { egara_storage_pool, egara_storage, [ { size, WorkerSize }, { max_overflow, 0 }], [ ] },
             { egara_ldap_pool, egara_ldap, [ { size, WorkerSize }, { max_overflow, 0 }], [ ] },
             { egara_imap_pool, egara_imap, [ { size, WorkerSize }, { max_overflow, 0 }], [ ] }
            ],
    PoolSpecs = lists:map(fun({Name, Module, PoolConfig, WorkerArgs}) ->
                                  PoolArgs = [{ name, { local, Name } }, { worker_module, Module }],
                                  poolboy:child_spec(Name, PoolArgs ++ PoolConfig, WorkerArgs)
                          end, Pools),
    %lager:info("Pools: ~p", [PoolSpecs]),
    Children = [
                 ?CHILD(egara_riak_config, worker),
                 ?CHILD(egara_notifications_receiver, worker),
                 ?CHILD(egara_notifications_processor, worker)
               ],
    {ok, { {one_for_one, 5, 10}, PoolSpecs ++ Children} }.

