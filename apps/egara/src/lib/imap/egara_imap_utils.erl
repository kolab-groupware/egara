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

-module(egara_imap_utils).
-export([extract_path_from_uri/3, extract_uidset_from_uri/1, mailbox_uid_header_name/0]).

%% Translate the folder name in to a fully qualified folder path such as it
%% would be used by a cyrus administrator.
extract_path_from_uri(SharedPrefix, HierarchyDelim, URI) when is_binary(URI) ->
    extract_path_from_uri(SharedPrefix, HierarchyDelim, binary_to_list(URI));
extract_path_from_uri(SharedPrefix, HierarchyDelim, URI) when is_list(URI) ->
    %%lager:info("Parsing ~p", [URI]),
    SchemeDefaults = [{ imap, 143 }, { imaps, 993 }],
    ParseOpts = [ { scheme_defaults, SchemeDefaults } ],
    Path = imap_folder_path(SharedPrefix, HierarchyDelim, http_uri:parse(URI, ParseOpts)),
    list_to_binary(Path).

extract_uidset_from_uri(URI) when is_binary(URI) ->
    { TagStart, TagEnd } = binary:match(URI, <<";UID=">>),
    UIDStart = TagStart + TagEnd + 1,
    case binary:match(URI, <<";">>, [{ scope, { UIDStart, -1 } }]) of
        nomatch -> binary:part(URI, UIDStart, -1);
        { Semicolon, _ } -> binary:part(URI, UIDStart, Semicolon - UIDStart)
    end.


mailbox_uid_header_name() -> <<"/vendor/cmu/cyrus-imapd/uniqueid">>.

%% Private

imap_folder_path_from_parts(none, _HierarchyDelim, none, _Domain, Path) ->
    Path;
imap_folder_path_from_parts(SharedPrefix, _HierarchyDelim, none, _Domain, Path) ->
    case SharedPrefix == string:chars(length(SharedPrefix), Path) of
        true -> string:sub_str(Path, length(SharedPrefix));
        _ -> Path
    end;
imap_folder_path_from_parts(_SharedPrefix, HierarchyDelim, User, Domain, "INBOX") ->
    string:join(["user", string:join([User, Domain], "@")], HierarchyDelim);
imap_folder_path_from_parts(_SharedPrefix, HierarchyDelim, User , Domain, Path) ->
    string:join(["user", User, string:join([Path, Domain], "@")], HierarchyDelim).

imap_folder_path(_SharedPrefix, _HierarchyDelim, { error, Reason }) ->
    lager:info("ERROR! ~p", [Reason]),
    bad_uri;
imap_folder_path(SharedPrefix, HierarchyDelim, { ok, {_Scheme, User, Domain, _Port, FullPath, _Query} }) ->
    { VDomain, _ImapHost } = split_imap_uri_domain(string:tokens(Domain, "@")),
    [ [_|Path] | _ ] = string:tokens(FullPath, ";"),
    %%lager:info("PARSED IMAP URI: ~p ~p ~p", [User, VDomain, Path]),
    CanonicalPath = imap_folder_path_from_parts(SharedPrefix, HierarchyDelim, User, VDomain, Path),
    %%lager:info("PUT TOGETHER AS: ~p", [CanonicalPath]),
    CanonicalPath.

split_imap_uri_domain([ ImapHost ]) -> { ImapHost, ImapHost };
split_imap_uri_domain([ VDomain, ImapHost ]) -> { VDomain, ImapHost }.

