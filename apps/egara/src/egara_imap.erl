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

-module(egara_imap).

-behaviour(gen_fsm).

%% API
-export([start_link/0, connect/1]).

%% gen_fsm callbacks
-export([disconnected/2, authenticate/2, authenticating/2, idle/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% state record definition
-record(state, { host, port, tls, user, pass, authed = false, socket = none, command_serial = 1  }).


%% public API
start_link() -> gen_fsm:start_link(?MODULE, [], []).
connect(PID) -> gen_fsm:send_event(PID, connect).

%% gen_server API
init(_Args) -> 
    Config = application:get_env(egara, cyrus, []),
    AdminConnConfig = proplists:get_value(admin_connection, Config, []),
    State = #state {
                host = proplists:get_value(host, AdminConnConfig, ""),
                port = proplists:get_value(port, AdminConnConfig, 993),
                tls = proplists:get_value(tls, AdminConnConfig, true),
                user = proplists:get_value(user, AdminConnConfig, "cyrus-admin"),
                pass = proplists:get_value(pass, AdminConnConfig, "")
              },
    { ok, disconnected, State }.

disconnected(connect, #state{ host = Host, port = Port, tls = TLS, socket = none } = State) ->
    lager:info("~p:  Connecting to ~p:~p", [connect, Host, Port]),
    {ok, Socket} = create_socket(Host, Port, TLS),
    { next_state, authenticate, State#state { socket = Socket } }.

authenticate({ data, Data }, #state{ socket = Socket, user = User, pass = Pass, authed = false } = State) ->
    { NextState, CommandTag } = generate_command_tag(State),
    lager:info("Got ~p", [Data]),
    lager:info("About to connect with ~p", [CommandTag]),
    send_command(Socket, io_lib:format("~s LOGIN ~s ~s~n", [CommandTag, User, Pass]), State),
    { next_state, authenticating, NextState#state{ authed = CommandTag} }.

authenticating({ data, Data}, #state{ authed = Tag } = State) when is_binary(Tag) ->
    Token = <<Tag/binary, " OK">>,
    Auth =
    case binary:match(Data, Token) of
        {0, _} ->
            lager:info("Logged in to IMAP server"),
            true;
        _ ->
            lager:warning("Log in to IMAP server failed: ~p", [Data]),
            false
    end,
    { next_state, idle, State#state{ authed = Auth } }.

idle({ data, Data }, State) ->
    lager:info("Idling, server sent: ~p", [Data]),
    { next_state, idle, State };
idle(_Event, State) ->
    lager:info("Idling"),
    { next_state, idle, State }.

handle_event(_Event, StateName, State) -> { next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) -> { next_state, StateName, State}.

handle_info({ ssl, Socket, Bin }, StateName, #state{ socket = Socket } = State) ->
    % Flow control: enable forwarding of next TCP message
    ssl:setopts(Socket, [{ active, once }]),
    ?MODULE:StateName({ data, Bin }, State);

handle_info({ tcp, Socket, Bin }, StateName, #state{ socket = Socket } = State) ->
    % Flow control: enable forwarding of next TCP message
    lager:info("Got us ~p", [Bin]),
    inet:setopts(Socket, [{ active, once }]),
    ?MODULE:StateName({ data, Bin }, State);

handle_info({tcp_closed, Socket}, _StateName, #state{ socket = Socket, host = Host } = State) ->
    lager:info("~p Client ~p disconnected.\n", [self(), Host]),
    { stop, normal, State };

handle_info(_Info, StateName, State) ->
        { noreply, StateName, State }.

terminate(_Reason, _Statename, _State) -> ok.

code_change(_OldVsn, Statename, State, _Extra) -> { ok, Statename, State }.

%% private API
tag_field_width(Serial) when Serial < 10000 -> 4;
tag_field_width(Serial) -> tag_field_width(Serial / 10000, 5).
tag_field_width(Serial, Count) when Serial < 10 -> Count;
tag_field_width(Serial, Count) -> tag_field_width(Serial / 10, Count + 1).

generate_command_tag(#state{ command_serial = Serial } = State) ->
    Tag = list_to_binary(io_lib:format("EG~*..0B", [tag_field_width(Serial), Serial])),
    { State#state{ command_serial = Serial + 1 }, Tag }.

create_socket(Host, Port, true) -> ssl:connect(Host, Port, [binary, {active, once}], 1000);
create_socket(Host, Port, _) -> gen_tcp:connect(Host, Port, [binary, {active, once}], 1000).

send_command(Socket, Command, #state{ tls = true}) -> lager:info("Sending command over SSL: ~p", [Command]), ssl:send(Socket, Command);
send_command(Socket, Command, _) -> lager:info("Sending command: ~s", [Command]), gen_tcp:send(Socket, Command).
