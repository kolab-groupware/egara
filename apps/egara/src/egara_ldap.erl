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

-module(egara_ldap).

-behaviour(gen_server).
-include_lib("eldap/include/eldap.hrl").

%% API
-export([ start_link/1,
          fetch_userdata_for_login/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% state record definition
%%TODO: support reconfiguration requests
-record(state, { base_dn, ldap_connection = none }).


%% public API
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).
fetch_userdata_for_login(Pid, UserLogin) -> gen_server:call(Pid, { fetch_userdata, UserLogin }).


%% gen_server API
init(_Args) ->
    process_flag(trap_exit, true),
    State = #state{ base_dn = proplists:get_value(base_dn, application:get_env(egara, ldap, []), [])},
    { ok, State }.

handle_call({ fetch_userdata, UserLogin } , From, State) when is_list(UserLogin) ->
    handle_call({ store_userdata, erlang:list_to_binary(UserLogin) }, From, State);
handle_call({ fetch_userdata, UserLogin }, _From, State) ->
    %%TODO perhaps call with continuation to avoid create new state everytime?
    NewState = ensure_connected(State),
    { reply, query_userdata(UserLogin, NewState), NewState };

handle_call(_Request, _From, State) ->
    { reply, ok, State }.

handle_cast(_Msg, State) ->
    { noreply, State }.

handle_info({'EXIT', From, _Reason}, State) ->
    %% look out for our ldap connection dropping
    if From =:= State#state.ldap_connection -> lager:warning("Just lost our ldap connection..."),
                                               { noreply, State#state{ ldap_connection = none } };
       true -> { noreply, State }
    end;
handle_info(_Info, State) ->
    { noreply, State }.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.


%% private API

%% LDAP CONNECTION ROUTINES
ensure_connected(#state{ ldap_connection = none } = State) ->
    %%TODO eldap:start_tls?
    LDAPConfig = application:get_env(egara, ldap, []),
    connect_with_config(proplists:get_value(hosts, LDAPConfig),
                        proplists:get_value(port, LDAPConfig),
                        proplists:get_value(bind_dn, LDAPConfig),
                        proplists:get_value(bind_pw, LDAPConfig),
                       State);
ensure_connected(State) ->
    State.

connect_with_config(Hosts, Port, BindDn, BindPw, State) when is_list(Hosts), is_integer(Port), is_list(BindDn), is_list(BindPw) ->
    ldap_connection_attempt(eldap:open(Hosts, [ { port, Port } ]), BindDn, BindPw, State);
connect_with_config(Hosts, _Port, BindDn, BindPw, State) when is_list(Hosts), is_list(BindDn), is_list(BindPw) ->
    ldap_connection_attempt(eldap:open(Hosts), BindDn, BindPw, State);
connect_with_config(_Hosts, _Port, _BindDn, _BindPw, State) ->
    lager:warning("LDAP configuration is broken!"),
    State.

ldap_connection_attempt({ ok, Handle }, BindDn, BindPw, State) ->
    ldap_bind_attempt(eldap:simple_bind(Handle, BindDn, BindPw), Handle, State);
ldap_connection_attempt({ error, Reason }, _BindDn, _BindPw, State) ->
    lager:error("LDAP: Could not connect to server: ~p", [Reason]),
    State.

ldap_bind_attempt(ok, Handle, State) ->
    link(Handle),
    State#state{ ldap_connection = Handle };
ldap_bind_attempt({ error, Reason }, Handle, State) ->
    lager:error("LDAP: Could not authenticate to server: ~p", [Reason]),
    eldap:close(Handle),
    State#state{ ldap_connection = none }.

%% QUERY FOR USER DATA
query_userdata(_UserLogin, #state{ ldap_connection = none }) ->
    notfound;
query_userdata(UserLogin, State) ->
    UserBaseDn = "ou=People," ++ State#state.base_dn,
    Attrs = ["cn", "nsuniqueid"], %%TODO nsuniqueid configurable?
    UserLoginString = erlang:binary_to_list(UserLogin),
    UserAuthAttrs = ["mail", "alias", "uid"], %%TODO: configurable?
    ObjectClassFilter = eldap:equalityMatch("objectclass", "inetorgperson"),
    UserFilter = eldap:'or'(lists:foldl(fun(Attr, Acc) -> [ eldap:equalityMatch(Attr, UserLoginString) | Acc ] end, [], UserAuthAttrs)),
    Filter = eldap:'and'([UserFilter, ObjectClassFilter]),
    SearchOptions = [{ base, UserBaseDn }, { filter, Filter }, { attributes, Attrs } ],
    %%lager:info("OUR FILTERS ARE ~p", [SearchOptions]),
    LDAPResponse = eldap:search(State#state.ldap_connection, SearchOptions),
    create_userdata_query_response(UserLogin, LDAPResponse).

create_userdata_query_response(UserLogin, { ok, #eldap_search_result{ entries = [] } }) ->
    lager:warning("LDAP: Could not find requested user ~p", [UserLogin]),
    notfound;
create_userdata_query_response(_UserLogin, { ok, #eldap_search_result{ entries = [ Entry | _ ] } }) ->
    %%TODO: if we get more than one match?
    LDAPAttributes = Entry#eldap_entry.attributes,
    %% TODO should it really be exporting ldap-specifics like "dn" and "cn"?
    [
      { <<"id">>, list_to_binary(hd(proplists:get_value("nsuniqueid", LDAPAttributes, ""))) },
      { <<"cn">>, list_to_binary(hd(proplists:get_value("cn", LDAPAttributes, ""))) },
      { <<"dn">>, list_to_binary(Entry#eldap_entry.object_name) }
    ];
create_userdata_query_response(UserLogin, { error, Reason }) ->
    lager:warning("LDAP: Could not find requested user ~p, reason: ~p", [UserLogin, Reason]),
    notfound;
create_userdata_query_response(_UserLogin, _) ->
    lager:warning("LDAP: unknown error on user query"),
    notfound.

%% UTILS
userlogin_to_current_userdata_key(UserLogin) when is_list(UserLogin) -> userlogin_to_current_userdata_key(erlang:list_to_binary(UserLogin));
userlogin_to_current_userdata_key(UserLogin) when is_binary(UserLogin) -> <<UserLogin/binary, "::current">>.

