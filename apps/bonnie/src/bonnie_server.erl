-module(bonnie_server).

-behaviour(gen_server).

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

start_link()    -> gen_server:start_link({local, ?MODULE}, ?MODULE, node(), []).  %%TODO: this should pass an list of the nodes for init/1
notification_received(Notification) -> gen_server:cast(?MODULE, { notification, Notification }).
poke()          -> poke(1).
poke(N)         -> gen_server:call(?MODULE, {poke, N}).
num_pokes()     -> gen_server:call(?MODULE, num_pokes).


%% gen_server callbacks
init(Nodes) ->
%%    notification_store:install(Nodes), %% FIXME:  move to supervisor?
    notification_store:start(),
    {ok, #state{}}.

handle_call(num_pokes, _From, State = #state{ num_pokes = PokeCount }) ->
    {reply, PokeCount, State};

handle_call({poke, N}, _From, State) ->
    NewPokeCount = State#state.num_pokes + N,
    NewState     = State#state{num_pokes = NewPokeCount},
    Reply        = {ok, NewPokeCount},
    {reply, Reply, NewState}.

handle_cast({ notification, Notification }, State) when is_binary(Notification) ->
    try jsx:decode(Notification) of
        Term -> notification_store:add(42, Term) %% FIXME: proper key
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
