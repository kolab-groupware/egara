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
-export([start_link/0, connect/1, disconnect/1, get_folder_annotations/3]).

%% gen_fsm callbacks
-export([disconnected/2, authenticate/2, authenticating/2, idle/2, wait_response/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% state record definition
-record(state, { host, port, tls, user, pass, authed = false, socket = none,
                 command_serial = 1, command_queue = queue:new(),
                 shared_prefix = none, hierarchy_delim = none }).
-record(command, { message, from }).


%% public API
start_link() -> gen_fsm:start_link(?MODULE, [], []).
connect(PID) -> gen_fsm:send_event(PID, connect).
disconnect(PID) -> gen_fsm:send_all_state_event(PID, disconnect).
get_folder_annotations(PID, From, Folder) when is_list(Folder) ->
    get_folder_annotations(PID, From, list_to_binary(Folder));
get_folder_annotations(PID, From, Folder) when is_binary(Folder) ->
    gen_fsm:send_all_state_event(PID, { get_folder_annotations, From, Folder }).

%% gen_server API
init(_Args) -> 
    Config = application:get_env(egara, cyrus, []),
    AdminConnConfig = proplists:get_value(admin_connection, Config, []),
    State = #state {
                host = proplists:get_value(host, AdminConnConfig, ""),
                port = proplists:get_value(port, AdminConnConfig, 993),
                tls  = proplists:get_value(tls, AdminConnConfig, true),
                user = list_to_binary(proplists:get_value(user, AdminConnConfig, "cyrus-admin")),
                pass = list_to_binary(proplists:get_value(pass, AdminConnConfig, ""))
              },
    gen_fsm:send_all_state_event(self(), { get_shared_prefix, self() }),
    { ok, disconnected, State }.

disconnected(connect, #state{ host = Host, port = Port, tls = TLS, socket = none } = State) ->
    lager:info("~p:  Connecting to ~p:~p", [connect, Host, Port]),
    {ok, Socket} = create_socket(Host, Port, TLS),
    { next_state, authenticate, State#state { socket = Socket } };
disconnected(connect, State) ->
    lager:warning("Already connected to IMAP server!"),
    { next_state, authenticate, State };
disconnected(Command, State) when is_record(Command, command) ->
    { next_state, disconnected, enque_command(Command, State) }.

authenticate({ data, _Data }, #state{ user = User, pass = Pass, authed = false } = State) ->
    NextState = send_command(<<"LOGIN ", User/binary, " ", Pass/binary>>, State),
    { next_state, authenticating, NextState#state{ authed = in_process } };
authenticate(Command, State) when is_record(Command, command) ->
    { next_state, authenticate, enque_command(Command, State) }.

authenticating({ data, Data }, #state{ authed = in_process } = State) ->
    Token = <<" OK ">>, %TODO would be nice to have the tag there
    case binary:match(Data, Token) of
        nomatch ->
            lager:warning("Log in to IMAP server failed: ~p", [Data]),
            close_socket(State),
            { next_state, idle, State#state{ socket = none, authed = false } };
        _ ->
            lager:info("Logged in to IMAP server successfully"),
            gen_fsm:send_event(self(), process_command_queue),
            { next_state, idle, State#state{ authed = true } }
    end;
authenticating(Command, State) when is_record(Command, command) ->
    { next_state, authenticating, enque_command(Command, State) }.

idle(process_command_queue, #state{ command_queue = Queue } = State) ->
    case queue:out(Queue) of
       { { value, #command{ message = Message, from = From } }, ModifiedQueue } ->
            lager:info("Clearing queue of ~p", [Message]),
            NextState = send_command(Message, State),
            { next_state, wait_response, NextState#state{ command_queue = ModifiedQueue } };
       { empty, ModifiedQueue } ->
            { next_state, idle, State#state{ command_queue = ModifiedQueue } }
    end;
idle({ data, Data }, State) ->
    lager:info("Idling, server sent: ~p", [Data]),
    { next_state, idle, State };
idle(#command{ message = Message, from = From }, State) ->
    lager:info("Idling"),
    NextState = send_command(Message, State),
    { next_state, wait_response, NextState };
idle(_Event, State) ->
    { next_state, idle, State }.

wait_response({ data, Data }, State) ->
    lager:info("Waiting for a response, server sent: ~p", [Data]),
    %%TODO: this case statement is going to quickly get ugly with all the message possibilities
    NewState =
    case Data of
        <<"* NAMESPACE ", _/binary>> ->
            { SharedPrefix, Delim } = egara_imap_parser_namespace:parse(Data),
            State#state{ shared_prefix = SharedPrefix, hierarchy_delim = Delim };
        _ -> State
    end,
    gen_fsm:send_event(self(), process_command_queue),
    { next_state, idle, NewState }.

handle_event(disconnect, _StateName, State) ->
    close_socket(State),
    { next_state, disconnected, reset_state(State) };
handle_event({ get_folder_annotations, From, Folder }, StateName, State) ->
    Command = #command{ message = <<"GETANNOTATION ", Folder/binary, " \"*\" \"value.shared\"">>, from = From },
    ?MODULE:StateName(Command, State);
handle_event({ get_shared_prefix, From }, StateName, State) ->
    %% http://tools.ietf.org/html/rfc2342
    Command = #command{ message = <<"NAMESPACE">>, from = From },
    ?MODULE:StateName(Command, State);
handle_event(_Event, StateName, State) -> { next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) -> { next_state, StateName, State}.

handle_info({ ssl, Socket, Bin }, StateName, #state{ socket = Socket } = State) ->
    % Flow control: enable forwarding of next TCP message
    lager:info("Received from server: ~p", [Bin]),
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
    { next_state, StateName, State }.

terminate(_Reason, _Statename, State) -> close_socket(State), ok.

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

close_socket(#state{ socket = none }) -> ok;
close_socket(#state{ socket = Socket, tls = true }) -> ssl:close(Socket);
close_socket(#state{ socket = Socket }) -> gen_tcp:close(Socket).

reset_state(State) -> State#state{ socket = none, authed = false, command_serial = 1 }.

send_command(Command, State) when is_list(Command) ->
    lager:info("Warning! Turning a command string into a binary!"),
    send_command(list_to_binary(Command), State);
send_command(Command, #state{ socket = none } = State) ->
    lager:warning("Not connected, dropping command on floor: ~s", [Command]),
    State;
send_command(Command, #state{ socket = Socket, tls = true} = State) ->
    { NextState, CommandTag } = generate_command_tag(State),
    Data = <<CommandTag/binary, " ", Command/binary, "\n">>,
    lager:info("Sending command over SSL: ~s", [Data]),
    ssl:send(Socket, Data),
    NextState;
send_command(Command, #state{ socket = Socket } = State) ->
    lager:info("Sending command: ~s", [Command]),
    { NextState, CommandTag } = generate_command_tag(State),
    gen_tcp:send(Socket, <<CommandTag, Command, "\n">>),
    NextState.

enque_command(Command, State) ->
    lager:info("Enqueuing command ~p", [Command]),
    State#state { command_queue = queue:in(Command, State#state.command_queue) }.

