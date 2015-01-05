-module(egara_notifications_receiver).

-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").

%% API
-export([ start_link/0
        , notification_received/1
        , poke/0
        , poke/1
        , num_pokes/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {num_pokes = 0}).
-define(PF_LOCAL, 1).
-define(SOCK_DGRAM, 2).
-define(UNIX_PATH_MAX, 108).

%% API

start_link()    -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
notification_received(Notification) -> gen_server:cast(?MODULE, { notification, Notification }).
poke()          -> poke(1).
poke(N)         -> gen_server:call(?MODULE, {poke, N}).
num_pokes()     -> gen_server:call(?MODULE, num_pokes).


%% gen_server callbacks
init([]) ->
    %% get the path to the listen socket, either from the app config
    %% or here
    case application:get_env(notification_socket_path) of
        Value when is_binary(Value) -> SocketPath = Value;
        _ -> SocketPath = <<"/tmp/egara-notify">>
    end,

    %% see if the file exists, and if it does, remove it if it is a socket
    %% allows to start the application multiple times, which is a good thing
    case file:read_file_info(SocketPath) of
        { ok, #file_info{ type = other } } -> ok = file:delete(SocketPath); %% TODO: handle errror with a report
        { ok, _ } -> notok; %% do not remove a non-socket file. TODO: handle errror with a report, clean exit
        {error, _} -> ok
    end,

    { ok, Socket } = procket:socket(?PF_LOCAL, ?SOCK_DGRAM, 0),
    Sun = <<?PF_LOCAL:16/native, % sun_family
            SocketPath/binary,   % address
            0:((?UNIX_PATH_MAX-byte_size(SocketPath))*8) %% zero out the rest
          >>,

    case procket:bind(Socket, Sun) of
        ok -> spawn(fun() -> recvNotification(Socket) end);
        { error, PosixError } -> lager:error("Could not bind to notification socket; error is: ~p", [PosixError])
    end,
    { ok, #state{} }.

recvNotification(Socket) ->
    case procket:recvfrom(Socket, 16#FFFF) of
        { error, eagain } ->
            timer:sleep(100),
            lager:info("EAGAIN!"),
            recvNotification(Socket);
        { ok, Buf } ->
            %%lager:info("~s", [binary_to_list(Buf)]),
            Components = binary:split(Buf, <<"\0">>, [global]),
            lager:info("~p", [Components]),
            recvNotification(Socket)
    end.

handle_call(num_pokes, _From, State = #state{ num_pokes = PokeCount }) ->
    {reply, PokeCount, State};

handle_call({poke, N}, _From, State) ->
    NewPokeCount = State#state.num_pokes + N,
    NewState     = State#state{num_pokes = NewPokeCount},
    Reply        = {ok, NewPokeCount},
    {reply, Reply, NewState}.

handle_cast({ notification, Notification }, State) when is_binary(Notification) ->
    try jsx:decode(Notification) of
        Term -> egara_notification_store:add(42, Term) %% FIXME: proper key
    catch
        error:_ -> ok %% Log it?
    end,
    { noreply, State };

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Upgrade from 2
code_change(_OldVsn, State, [from2to3]) ->
    error_logger:info_msg("CODE_CHANGE from 2~n"),
    {state, NumPokes} = State, %% State here is the 'old' format, with 1 field
    NewState = #state{num_pokes=NumPokes}, %% will assume default for num_prods
    {ok, NewState}.

%% Note downgrade code_change not implemented
    
    

%%% Internal functions
