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

%% API

start_link()    -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
notification_received(Notification) -> gen_server:cast(?MODULE, { notification, Notification }).
poke()          -> poke(1).
poke(N)         -> gen_server:call(?MODULE, {poke, N}).
num_pokes()     -> gen_server:call(?MODULE, num_pokes).


%% gen_server callbacks
init([]) ->
    Options = [{ active, true }, binary],

    %% get the path to the listen socket, either from the app config
    %% or here
    case application:get_env(notification_socket_path) of
        undefined -> ListenPath = "/tmp/egara-notify";
        Value -> ListenPath = Value
    end,

    %% see if the file exists, and if it does, remove it if it is a socket
    %% allows to start the application multiple times, which is a good thing
    case file:read_file_info(ListenPath) of
        { ok, #file_info{ type = other } } -> ok = file:delete(ListenPath); %% TODO: handle errror with a report
        { ok, _ } -> notok; %% do not remove a non-socket file. TODO: handle errror with a report, clean exit
        {error, _} -> ok
    end,

    { ok, Listen } = afunix:listen(ListenPath, Options),
    spawn(fun() -> acceptNotifiers(Listen) end),
    { ok, #state{} }.

acceptNotifiers(Listen) ->
    { ok, Socket } = gen_tcp:accept(Listen),
    spawn(fun() -> acceptNotifiers(Listen) end),
    handle(Socket).

handle(Socket) ->
    inet:setopts(Socket, [{ active, once }, binary ]),
    receive
        { _, Socket, { socket_closed, normal } } ->
            ok;   %% socket closed on connecting side
        { _, Socket, { socket_closed, Error } } ->
            lager:warning("Lost notification connection due to %p", Error),
            ok; %% error! not so hot...
        { _, Socket, Msg } ->
            gen_tcp:send(Socket, Msg),
            handle(Socket)
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
