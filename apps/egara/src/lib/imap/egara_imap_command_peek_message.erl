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

-module(egara_imap_command_peek_message).
-export([new/1, parse/2, continue_parse/3]).
-record(parse_state, { body_size, results, data }).

%% https://tools.ietf.org/html/rfc3501#section-6.4.5

%% Public API
new(MessageID) when is_integer(MessageID) -> new(integer_to_binary(MessageID));
new(MessageID) when is_binary(MessageID) -> <<"UID FETCH ",  MessageID/binary, " (FLAGS BODY.PEEK[HEADER] BODY.PEEK[TEXT])">>.

continue_parse(Data, _Tag, #parse_state{ body_size = Size, results = Results, data = PrevData }) ->
    try_body_parse(<<Data/binary, PrevData/binary>>, Size, Results).

parse(Data, Tag) when is_binary(Data) ->
    case egara_imap_utils:check_response_for_failure(Data, Tag) of
        ok -> get_past_headers(Data);
        { _, Reason } -> log_error(Reason), { fini, error }
    end.

%% Private API
log_error(Reason) -> lager:error("Could not fetch message: ~p", [Reason]).

get_past_headers(<<" OK ", Data/binary>>) -> { fini, [] };
get_past_headers(<<" FETCH ", Data/binary>>) -> find_open_parens(Data);
get_past_headers(<<_, Data/binary>>) -> get_past_headers(Data);
get_past_headers(<<>>) -> { error, <<"Unparsable">> }.

find_open_parens(<<$(, Data/binary>>) -> parse_next_component(Data, []);
find_open_parens(<<_, Data/binary>>) -> find_open_parens(Data).

parse_next_component(<<"FLAGS (", Data/binary>>, Results) ->
    parse_flags(Data, Results);
parse_next_component(<<"BODY[HEADER] {", Data/binary>>, Results) ->
    parse_header(Data, Results);
parse_next_component(<<"BODY[TEXT] {", Data/binary>>, Results) ->
    parse_body(Data, Results);
parse_next_component(<<_, Data/binary>>, Results) -> parse_next_component(Data, Results);
parse_next_component(<<"OK Completed", _/binary>>, Results) -> Results;
parse_next_component(<<>>, Results) -> { fini, Results }.

parse_flags(Data, Results) -> parse_flags(Data, Results, Data, 0).
parse_flags(OrigData, Results, <<$), Rest/binary>>, Length) ->
    FlagString = binary:part(OrigData, 0, Length),
    Flags = binary:split(FlagString, <<" ">>, [global]),
    ResultsWithFlags = [{ flags, Flags } | Results],
    parse_next_component(Rest, ResultsWithFlags);
parse_flags(OrigData, Results, <<_, Data/binary>>, Length) ->
    parse_flags(OrigData, Results, Data, Length + 1);
parse_flags(_OrigData, Results, <<>>, _Length) ->
    { fini, Results }.

filter_headers(RawHeaders) ->
    filter_headers(RawHeaders, none, none, []).

filter_headers([], CurrentKey, CurrentValue, Acc) ->
    filter_header_add(CurrentKey, CurrentValue, Acc);
filter_headers([<<>>|Headers], CurrentKey, CurrentValue, Acc) ->
    filter_headers(Headers, CurrentKey, CurrentValue, Acc);
filter_headers([Header|Headers], CurrentKey, CurrentValue, Acc) ->
    filter_header(Headers, CurrentKey, CurrentValue, Acc, binary:split(Header, <<": ">>)).

filter_header(Headers, CurrentKey, CurrentValue, Acc, [Key, Value]) ->
    Added = filter_header_add(CurrentKey, CurrentValue, Acc),
    filter_headers(Headers, Key, Value, Added);
filter_header(Headers, none, _CurrentValue, Acc, _Value) ->
    filter_headers(Headers, none, none, Acc);
filter_header(Headers, CurrentKey, CurrentValue, Acc, [Value]) ->
    %%TODO: get rid of tab, ensure "proper" whitespace
    NewValue = <<CurrentValue/binary, Value/binary>>,
    filter_headers(Headers, CurrentKey, NewValue, Acc).

filter_header_add(CurrentKey, CurrentValue, Acc) when CurrentKey =/= none, CurrentValue =/= none ->
    %%TODO: split CurrentValue on ','
    [ { CurrentKey, CurrentValue } | Acc ];
filter_header_add(_, _, Acc) ->
    Acc.

parse_header(Data, Results) -> parse_header(Data, Results, Data, 0).
parse_header(_OrigData, Results, <<$}, Rest/binary>>, 0) ->
    parse_next_component(Rest, Results);
parse_header(OrigData, Results, <<$}, Rest/binary>>, Length) ->
    ByteSizeString = binary:part(OrigData, 0, Length),
    Size = binary_to_integer(ByteSizeString),
    HeaderString = binary:part(Rest, 2, Size), %% the 2 is for \r\n
    RawHeaders = binary:split(HeaderString, <<"\r\n">>, [global]),
    Headers = filter_headers(RawHeaders),
    %%FIXME: make sure we have enough data loaded, otherwise continue
    Remainder = binary:part(Rest, Size, byte_size(Rest) - Size),
    ResultsWithHeaders = [{ headers, Headers} | Results],
    parse_next_component(Remainder, ResultsWithHeaders);
parse_header(OrigData, Results, <<_, Rest/binary>>, Length) ->
    parse_header(OrigData, Results, Rest, Length + 1);
parse_header(_OrigData, Results, <<>>, _Length) ->
    { fini, Results }.

parse_body(Data, Results) -> parse_body(Data, Results, Data, 0).
parse_body(_OrigData, Results, <<$}, Rest/binary>>, 0) ->
    parse_next_component(Rest, Results);
parse_body(OrigData, Results, <<$}, Rest/binary>>, Length) ->
    ByteSizeString = binary:part(OrigData, 0, Length),
    Size = binary_to_integer(ByteSizeString),
    %%lager:info("We have ... ~p ~p ~p", [ByteSizeString, Size, byte_size(Rest)]),
    try_body_parse(Rest, Size, Results);
parse_body(OrigData, Results, <<_, Rest/binary>>, Length) ->
    parse_body(OrigData, Results, Rest, Length + 1);
parse_body(_OrigData, Results, <<>>, _Length) ->
    { fini, Results }.

try_body_parse(Data, Size, Results) ->
    case Size > byte_size(Data) of
        true ->
            { more, fun ?MODULE:continue_parse/3, #parse_state{ body_size = Size, results = Results, data = Data } };
        false ->
            Body = binary:part(Data, 2, Size), %% the 2 is for \r\n
            Remainder = binary:part(Data, Size, -1),
            parse_next_component(Remainder, [{ body, Body } | Results])
    end.
