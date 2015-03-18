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

-module(egara_imap_command_annotation).
-export([new/1, parse/2]).

%% Public API
new(Folder) when is_binary(Folder) ->
    Message = <<"GETANNOTATION \"", Folder/binary, "\" \"*\" \"value.shared\"">>.

parse(Data, _Tag) when is_binary(Data) ->
    Lines = binary:split(Data, <<"\r\n">>, [global]),
    { fini, lists:foldl(fun parseLine/2, [], Lines) }.


%% Private API
parseLine(<<"* ANNOTATION", Data/binary>>, Acc) ->
    Pieces = binary:split(Data, <<"\"">>, [global]),
    process_pieces(Pieces, Acc);
parseLine(<<>>, Acc) ->
    Acc;
parseLine(Data, Acc) ->
    handle_possible_end(binary:split(Data, <<" ">>), Acc).

process_pieces([_MBox, Key, _, _, _, Value, _], Acc) -> [ { Key, translate(Value) } | Acc ];
process_pieces(_, Acc) -> Acc.

translate(<<"false">>) -> false;
translate(<<"true">>) -> true;
translate(Value) ->
    try list_to_integer(binary_to_list(Value)) of
        Number -> Number
    catch
        _:_ -> Value
    end.

handle_possible_end(<<"OK", _/binary>>, Acc) ->
    Acc;
handle_possible_end(<<"BAD ", Reason/binary>>, Acc) ->
    lager:warning("Annotation error from imap server: ~p", [Reason]),
    Acc;
handle_possible_end([_Tag, Message], Acc) ->
    handle_possible_end(Message, Acc);
handle_possible_end(Message, Acc) ->
    lager:warning("Unexpected response from imap server: ~p", [Message]),
    Acc.
