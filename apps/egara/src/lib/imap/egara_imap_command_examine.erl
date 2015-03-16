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

-module(egara_imap_command_examine).
-export([new/1, parse/2]).

%% https://tools.ietf.org/html/rfc3501#section-6.3.2

%% Public API
new(MBox) when is_binary(MBox) -> <<"EXAMINE ", MBox/binary>>.

parse(Data, Tag) when is_binary(Data) ->
    NoToken = <<Tag/binary, " NO">>,
    NoTokenLength = byte_size(NoToken),
    is_no_token_found(Data, Tag, binary:match(Data, NoToken, [ { scope, { 0, NoTokenLength } } ])).

is_no_token_found(Data, Tag, nomatch) ->
    BadToken = <<Tag/binary, " BAD">>,
    BadTokenLength = byte_size(BadToken),
    is_bad_token_found(Data, Tag, binary:match(Data, BadToken, [ { scope, { 0, BadTokenLength } } ]));
is_no_token_found(Data, _Tag, _) ->
    lager:error("Could not examine folder: ~p", [Data]),
    { fini, error }.

is_bad_token_found(Data, Tag, nomatch) ->
    { finis, ok };
is_bad_token_found(Data, _Tag, _) ->
    lager:error("Could not examine folder: ~p", [Data]),
    { finis, error }.

%% Private API

