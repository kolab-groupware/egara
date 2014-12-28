-module(bonnie_server).

-behaviour(gen_server).

%% API
%% API
-export([ start_link/0
        , poke/0
        , poke/1
        , poke_twice/0
        , num_pokes/0
        , prod/0
        , prod/1
        , num_prods/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3, baseline/1]).

-record(state, {num_pokes = 0, num_prods = 0}).

%% API
start_link()    -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

baseline(N) ->
    gen_server:call(?MODULE, {baseline, N}).

spin(ParentPid, Number) ->
    %%io:fwrite("Hoi = ~p~n", [ParentPid]),
    ParentPid ! { completed, Number - 1 },
    exit(self()).

poke()          -> poke(1).
poke_twice()    -> poke(2).
poke(N)         -> gen_server:call(?MODULE, {poke, N}).

prod()          -> prod(1).
prod(N)         -> gen_server:call(?MODULE, {prod, N}).

num_pokes()     -> gen_server:call(?MODULE, num_pokes).
num_prods()     -> gen_server:call(?MODULE, num_prods).


%% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call({baseline, N}, _From, State) ->
    SPid = self(),
    io:fwrite("Spawning ~B procs from process ~p at ~w~n", [N, SPid, now()]),
    spawn(fun() -> spin(SPid, N) end),
    {reply, true, State};

handle_call(num_pokes, _From, State = #state{ num_pokes = PokeCount }) ->
    {reply, PokeCount, State};

handle_call(num_prods, _From, State = #state{ num_prods = ProdCount }) ->
    {reply, ProdCount, State};

handle_call({prod, N}, _From, State) ->
    NewProdCount = State#state.num_prods + N,
    NewState     = State#state{num_prods = NewProdCount},
    Reply        = {ok, NewProdCount},
    {reply, Reply, NewState};

handle_call({poke, N}, _From, State) ->
    NewPokeCount = State#state.num_pokes + N,
    NewState     = State#state{num_pokes = NewPokeCount},
    Reply        = {ok, NewPokeCount},
    {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({completed, 0}, State) ->
    io:fwrite("completed at ~w~n", [now()]),
    {noreply, State};

handle_info({completed, N}, State) ->
    %%io:fwrite("completed~n"),
    %%if N rem 1000 =:= 0 -> io:fwrite("completed ~B~n", [N]); true -> true end,
    SPid = self(),
    spawn(fun() -> spin(SPid, N) end),
    {noreply, State};

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
