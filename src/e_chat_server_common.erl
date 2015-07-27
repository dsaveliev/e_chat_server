-module(e_chat_server_common).

-export([md5/1]).
-export([format/2]).
-export([match/2]).
-export([is_uuid/1]).

md5(S) ->
    Hash = lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)]),
    HashToUpper = string:to_upper(Hash),
    list_to_binary(HashToUpper).

format(Template, Variables) ->
    IOList = io_lib:format(Template, Variables),
    lists:flatten(IOList).

match(String, RegExp) ->
    case re:run(String, RegExp) of
        {match, _} -> true;
        _ -> false
    end.

is_uuid(String) ->
  RegExp = "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$",
  match(String, RegExp).
