-module(e_chat_server_common).

-export([md5/1]).
-export([format/2]).
-export([match/2]).
-export([is_uuid/1]).
-export([iso8601/1]).
-export([normalize_id/1]).

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

iso8601(Date) ->
  {{Year,Month,Day},{Hours,Minutes,Seconds}} = Date,
  Date2 = format("~B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [Year,Month,Day,Hours,Minutes,round(Seconds)]),
  list_to_binary(Date2).

normalize_id(Id) ->
    case is_binary(Id) of
        true -> binary_to_integer(Id);
        false -> Id
    end.
