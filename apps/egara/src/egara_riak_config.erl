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

-module(egara_riak_config).

-behaviour(gen_server).

%% API
-export([start_link/0, connection_params/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% state record definition
-record(state, { pos = 1, nodes }).


%% public API
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
connection_params() -> gen_server:call(?MODULE, connection_params).

%% gen_server API
init(_Args) -> 
    Config = application:get_env(egara, riak, []),
    Nodes = sanitize_node_config(proplists:get_value(nodes, Config, missing)),
    State = #state{ nodes = Nodes },
    BucketTypes = proplists:get_value(bucket_types, Config, []),
    { Host, Port } = erlang:hd(Nodes),
    { ok, Connection } = riakc_pb_socket:start_link(Host, Port),
    setup_bucket_types(Connection, BucketTypes),
    riakc_pb_socket:stop(Connection),
    { ok, State }.

handle_call(connection_params, _From, State) ->
    next_node(State);
handle_call(_, _, State) -> { reply, ok, State}.

handle_cast(_Msg, State) -> { noreply, State }.

handle_info(_Info, State) -> { noreply, State }.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> { ok, State }.

%% private API
sanitize_node_config(List) when is_list(List) ->
    Sanitizer = fun({ Host, Port } = Tuple, Acc) when is_list(Host), is_integer(Port) -> [ Tuple | Acc ];
                   (_, Acc) -> Acc
                end,
    case lists:foldl(Sanitizer, [], List) of
        [] -> sanitize_node_config(missing);
        Config -> Config
    end;
sanitize_node_config(missing) -> [ { "localhost", 10017 } ].

next_node(State) when is_record(State, state) -> next_node(length(State#state.nodes), State).
next_node(Length, #state{ pos = Pos, nodes = Nodes } = State) when Pos > length(State#state.nodes) ->
    [ Tuple | _ ] = Nodes,
    { reply, Tuple, State#state{ pos = 2 } };
next_node(_Length, #state{ pos = Pos, nodes = Nodes } = State) ->
    Tuple = lists:nth(Pos, Nodes),
    { reply, Tuple, State#state{ pos = Pos + 1 } }.

%%TODO: current almost entirely useless since the API does not allow creating new bucket types
setup_bucket_types(Connection, [{ Bucket, Properties } | Tail]) -> 
    CurrentTypeProperties = riakc_pb_socket:get_bucket_type(Connection, Bucket),
    case CurrentTypeProperties of
        { ok, _ } -> %% TODO check that the properties actually match up so as not to set the type up EVERY time
            riakc_pb_socket:set_bucket_type(Connection, Bucket, Properties);
        { error, _ } ->
            lager:info("Missing Riak bucket type ~p, intended to have properties ~p", [Bucket, CurrentTypeProperties]),
            riakc_pb_socket:set_bucket_type(Connection, Bucket, Properties)
    end,
    setup_bucket_types(Connection, Tail);
setup_bucket_types(_Connection, []) -> ok.

