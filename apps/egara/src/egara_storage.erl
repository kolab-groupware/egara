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
-export([ start_link/0,
          store_notification/3,
          store_userdata/3,
          store_message_history_entry/3,
          fetch_userdata_for_login/2,
          store_folder_uid/3,
          fetch_folder_uid/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% state record definition
-record(state, { riak_connection = none }).


%% public API
start_link() -> gen_server:start_link(?MODULE, [], []).
store_notification(Pid, Key, Notification) -> gen_server:call(Pid, { store_notification, Key, Notification }).
store_userdata(Pid, UserLogin, UserData) -> gen_server:call(Pid, { store_userdata, UserLogin, UserData }).
fetch_userdata_for_login(Pid, UserLogin) -> gen_server:call(Pid, { fetch_userdata, UserLogin }).
store_folder_uid(Pid, Folder, UID) when is_binary(Folder), is_binary(UID) -> gen_server:call(Pid, { store_folder_uid, Folder, UID }).
fetch_folder_uid(Pid, Folder) when is_binary(Folder) -> gen_server:call(Pid, { fetch_folder_uid, Folder }).
store_message_history_entry(Pid, Key, Value) when is_binary(Key), is_binary(Value) -> gen_server:call(Pid, { store_message_history_entry, Key, Value }).

%% gen_server API
init(_) ->
    erlang:process_flag(trap_exit, true),
    { ok, #state {} }.

handle_call({ store_notification, Keys, Notification }, _From, State) when is_list(Keys) ->
    NewState = ensure_connected(State),
    Json = jsx:encode(Notification),
    store_notification_json(Keys, Json, NewState, {});
handle_call({ store_notification, Key, Notification }, _From, State) when is_binary(Key) ->
    %%lager:info("Notification----> ~p = ~p", [Key, Notification]),
    NewState = ensure_connected(State),
    Json = jsx:encode(Notification),
    store_notification_json(Key, Json, NewState);

handle_call({ store_userdata, UserLogin, UserData }, From, State) when is_list(UserLogin) ->
    handle_call({ store_userdata, erlang:list_to_binary(UserLogin), UserData }, From, State);
handle_call({ store_userdata, UserLogin, UserData }, _From, State) ->
    UserId = proplists:get_value(<<"id">>, UserData, ""),
    TS = erlang:list_to_binary(egara_utils:current_timestamp()),
    Key = <<UserId/binary, "::", TS/binary, "::", UserLogin/binary>>,
    Json = jsx:encode([ { <<"user">>, UserLogin } | UserData ]),
    Storable = riakc_obj:new(historical_users_bucket(), Key, Json, json_type()),
    NewState = ensure_connected(State),
    case riakc_pb_socket:put(NewState#state.riak_connection, Storable) of
        ok -> 
            CurrentStorable = riakc_obj:new(current_users_bucket(), UserLogin, Json, json_type()),
            case riakc_pb_socket:put(NewState#state.riak_connection, CurrentStorable) of
                ok -> { reply, ok, NewState };
                Rv -> lager:warning("Failed to store current user data: ~p", [Rv]), { reply, error, NewState }
            end;
        Rv -> lager:warning("Failed to store user data: ~p", [Rv]), { reply, error, NewState }
    end;

handle_call({ fetch_userdata, UserLogin }, From, State) when is_list(UserLogin) ->
    handle_call({ store_userdata, erlang:list_to_binary(UserLogin) }, From, State);
handle_call({ fetch_userdata, UserLogin }, _From, State) when is_binary(UserLogin) ->
    NewState = ensure_connected(State),
    RiakResponse = riakc_pb_socket:get(NewState#state.riak_connection, current_users_bucket(), UserLogin),
    Response =
    case RiakResponse of
        { ok, Obj } ->
            Value = riakc_obj:get_value(Obj),
            try jsx:decode(Value) of
                Term -> Term
            catch
                error:_ -> notfound
            end;
        _ -> notfound
    end,
    { reply, Response, NewState };

handle_call({ store_folder_uid, Folder, UID }, _From, State) when is_binary(Folder), is_binary(UID) ->
    NewState = ensure_connected(State),
    Storable = riakc_obj:new(current_folders_bucket(), Folder, UID),
    Rv = riakc_pb_socket:put(NewState#state.riak_connection, Storable),
    { reply, Rv, NewState };

handle_call({ fetch_folder_uid, Folder }, From, State) when is_list(Folder) ->
    handle_call({ fetch_folder_uid, list_to_binary(Folder) }, From, State);
handle_call({ fetch_folder_uid, Folder }, _From, State) when is_binary(Folder) ->
    NewState = ensure_connected(State),
    RiakResponse = riakc_pb_socket:get(NewState#state.riak_connection, current_folders_bucket(), Folder),
    Response =
    case RiakResponse of
        { ok, Obj } -> riakc_obj:get_value(Obj);
        _ -> notfound
    end,
    { reply, Response, NewState };

handle_call({ store_message_history_entry, Key, Value}, _From, State) when is_binary(Key), is_binary(Value) ->
    %%lager:info("Storing history entry ~p => ~p", [Key, Value]),
    NewState = ensure_connected(State),
    Storable = riakc_obj:new(message_timeline_bucket(), Key, Value),
    Rv = riakc_pb_socket:put(NewState#state.riak_connection, Storable),
    { reply, Rv, NewState };


handle_call(_Request, _From, State) ->
    { reply, ok, State }.

handle_cast(_Msg, State) ->
    { noreply, State }.

handle_info({'EXIT', _ParentPid, shutdown}, _State) ->
    exit(shutdown);
handle_info({'EXIT', From, _Reason}, State) ->
    %% look out for our riak connection dropping
    if From =:= State#state.riak_connection -> lager:warning("Just lost our riak connection..."),
                                               { noreply, State#state{ riak_connection = none } };
       true -> { noreply, State }
    end;
handle_info(_Info, State) ->
    { noreply, State }.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.


%% private API
ensure_connected(#state{ riak_connection = none } = State) ->
    { Host, Port } = egara_riak_config:connection_params(),
    %%lager:info("Going to try with ... ~p ~p ~p", [Host, Port, State#state.riak_connection]),
    case riakc_pb_socket:start_link(Host, Port) of
        { ok, Connection } -> State#state{ riak_connection = Connection };
        Actual -> lager:warning("COULD NOT CONNECT TO RIAK! Reason: ~p", [Actual]), State
    end;
ensure_connected(State) ->
    State.

store_notification_json([], _Json, _State, Reply) ->
    Reply;
store_notification_json([Key|Keys], Json, State, _ReplyPlaceholder) ->
    Reply = store_notification_json(Key, Json, State),
    store_notification_json(Keys, Json, State, Reply).
store_notification_json(Key, Json, State) when is_binary(Key) ->
    %%lager:info("Going to store ~p", [Key]),
    Storable = riakc_obj:new(notification_bucket(), Key, Json, json_type()),
    case riakc_pb_socket:put(State#state.riak_connection, Storable) of
        ok -> { reply, ok, State };
        Rv -> lager:warning("Failed to put notification: ~p", [Rv]), { reply, error, State }
    end.


historical_users_bucket() -> { <<"egara-lww">>, <<"users">> }.
current_users_bucket() -> { <<"egara-unique">>, <<"users-current">> }.
notification_bucket() -> { <<"egara-lww">>, <<"imap-events">> }.
%%historical_folders_bucket() -> { <<"egara-lww">>, <<"imap-folders">> }.
current_folders_bucket() -> { <<"egara-unique">>, <<"imap-folders-current">> }.
message_timeline_bucket() -> { <<"egara-lww">>, <<"imap-message-timeline">> }.
json_type() -> <<"application/json">>.
