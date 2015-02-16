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

-module(egara_storage).

-behaviour(gen_server).

%% API
-export([ start_link/1,
          store_notification/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% state record definition
-record(state, { riak_connection = none }).


%% public API
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).
store_notification(Pid, Key, Notification) -> gen_server:call(Pid, { store_notification, Key, Notification }).


%% gen_server API
init(_Args) ->
    { ok, #state {} }.

handle_call({ store_notification, Key, Notification }, _From, State) when is_binary(Key) ->
    Json = jsx:encode(Notification),
    Storable = riakc_obj:new("notifications", Key, Json),
    NewState = ensure_connected(State),
    ok = riakc_pb_socket:put(NewState#state.riak_connection, Storable),
    { reply, ok, NewState };

handle_call(_Request, _From, State) ->
    { reply, ok, State }.

handle_cast(_Msg, State) ->
    { noreply, State }.

handle_info(_Info, State) ->
    { noreply, State }.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.


%% private API
ensure_connected(#state{ riak_connection = none } = State) ->
    { ok, Host } = application:get_env(egara, riak_host, "127.0.0.1"),
    { ok, Port } = application:get_env(egara, riak_port, 8087),
    { ok, Connection } = riakc_pb_socket:start_link(Host, Port),
    State#state{ riak_connection = Connection };
ensure_connected(#state{} = State) ->
    State.

