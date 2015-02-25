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
          store_notification/3,
          store_userdata/3,
          fetch_userdata_for_login/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% state record definition
-record(state, { riak_connection = none }).


%% public API
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).
store_notification(Pid, Key, Notification) when is_binary(Key) -> gen_server:call(Pid, { store_notification, Key, Notification }).
store_userdata(Pid, UserLogin, UserData) -> gen_server:call(Pid, { store_userdata, UserLogin, UserData }).
fetch_userdata_for_login(Pid, UserLogin) -> gen_server:call(Pid, { fetch_userdata, UserLogin }).


%% gen_server API
init(_Args) ->
    erlang:process_flag(trap_exit, true),
    { ok, #state {} }.

handle_call({ store_notification, Key, Notification }, _From, State) ->
    Json = jsx:encode(Notification),
    Storable = riakc_obj:new(<<"notifications">>, Key, Json),
    NewState = ensure_connected(State),
    case riakc_pb_socket:put(NewState#state.riak_connection, Storable) of
        ok -> { reply, ok, NewState };
        Rv -> lager:warning("Failed to put notification: ~p", [Rv]), { reply, error, NewState }
    end;

handle_call({ store_userdata, UserLogin, UserData }, From, State) when is_list(UserLogin) ->
    handle_call({ store_userdata, erlang:list_to_binary(UserLogin), UserData }, From, State);
handle_call({ store_userdata, UserLogin, UserData }, _From, State) ->
    UserId = proplists:get_value(<<"id">>, UserData, <<"">>),
    TS = erlang:list_to_binary(egara_utils:current_timestamp()),
    Key = <<UserLogin/binary, "::", TS/binary, "::", UserId/binary>>,
    CurrentKey = userlogin_to_current_userdata_key(UserLogin),
    Json = jsx:encode(UserData ++ [ { <<"user">>, UserLogin } ]),
    Storable = riakc_obj:new(<<"users">>, Key, Json),
    CurrentStorable = riakc_obj:new(<<"users">>, CurrentKey, Json),
    NewState = ensure_connected(State),
    Rv = riakc_pb_socket:put(NewState#state.riak_connection, Storable),
    RvCurrent = riakc_pb_socket:put(NewState#state.riak_connection, CurrentStorable),
    { reply, Rv, NewState };

handle_call({ fetch_userdata, UserLogin } , From, State) when is_list(UserLogin) ->
    handle_call({ store_userdata, erlang:list_to_binary(UserLogin) }, From, State);
handle_call({ fetch_userdata, UserLogin }, _From, State) ->
    NewState = ensure_connected(State),
    CurrentKey = userlogin_to_current_userdata_key(UserLogin),
    RiakResponse = riakc_pb_socket:get(NewState#state.riak_connection, <<"users">>, CurrentKey),
    case RiakResponse of
        { ok, Obj } ->
            Value = riakc:get_value(Obj),
            Response =
            try jsx:decode(Value) of
                Term -> Term
            catch
                error:_ -> notfound
            end;
        _ -> Response = notfound
    end,
    { reply, Response, NewState };

handle_call(_Request, _From, State) ->
    { reply, ok, State }.

handle_cast(_Msg, State) ->
    { noreply, State }.

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
    Host = application:get_env(egara, riak_host, "127.0.0.1"),
    Port = application:get_env(egara, riak_port, 8087),
    %%lager:info("Going to try with ... ~p ~p ~p", [Host, Port, State#state.riak_connection]),
    case riakc_pb_socket:start_link(Host, Port) of
        { ok, Connection } -> State#state{ riak_connection = Connection };
        Actual -> lager:warning("COULD NOT CONNECT TO RIAK! Reason: ~p", [Actual]), State
    end;
ensure_connected(State) ->
    State.

userlogin_to_current_userdata_key(UserLogin) when is_list(UserLogin) -> userlogin_to_current_userdata_key(erlang:list_to_binary(UserLogin));
userlogin_to_current_userdata_key(UserLogin) when is_binary(UserLogin) -> <<UserLogin/binary, "::current">>.

