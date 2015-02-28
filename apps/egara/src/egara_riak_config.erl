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
-record(state, { pos = 1, config }).


%% public API
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
connection_params() -> gen_server:call(?MODULE, connection_params).

%% gen_server API
init(_Args) -> 
    { ok, #state{ config = sanitize_config(application:get_env(egara, riak, missing)) } }.

handle_call(connection_params, _From, State) ->
    Length = length(State#state.config),
    next_config(Length, State);
handle_call(_, _, State) -> { reply, ok, State}.

handle_cast(_Msg, State) -> { noreply, State }.

handle_info(_Info, State) -> { noreply, State }.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> { ok, State }.

%% private API
sanitize_config(List) when is_list(List) ->
    Sanitizer = fun({ Host, Port } = Tuple, Acc) when is_list(Host), is_integer(Port) -> [ Tuple | Acc ];
                   (_, Acc) -> Acc
                end,
    case lists:foldl(Sanitizer, [], List) of
        [] -> sanitize_config(missing);
        Config -> Config
    end;
sanitize_config(_) -> [ { "localhost", 10017 } ].

next_config(Length, #state{ pos = Pos, config = Config } = State) when Pos > Length ->
    [ Tuple | _ ] = Config,
    { reply, Tuple, State#state{ pos = 2 } };
next_config(_Length, #state{ pos = Pos, config = Config } = State) ->
    Tuple = lists:nth(Pos, Config),
    { reply, Tuple, State#state{ pos = Pos + 1 } }.
