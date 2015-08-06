-module(e_chat_server_message_model).

-export([create/1]).
-export([find/1]).
-export([find_all/1]).
-export([is_exists/1]).
-export([render/1]).

-include("../e_chat_server_models.hrl").

create(Data) ->
    [{user_id, RawUserId}, {room_id, RawRoomId}, {text, Text}] = Data,
    UserId = e_chat_server_common:normalize_id(RawUserId),
    RoomId = e_chat_server_common:normalize_id(RawRoomId),
    Template = "INSERT INTO messages (user_id,room_id,text) VALUES ('~w','~w','~s')",
    Request = e_chat_server_common:format(Template, [UserId, RoomId, Text]),
    {ok, 1} = pgapp:equery(pgpool, Request, []),
    find(Data).

find(Data) ->
    case find_all(Data) of
        [] -> undefined;
        Messages -> lists:last(Messages)
    end.

find_all(Data) ->
    case Data of
        [{user_id, RawUserId}, {room_id, RawRoomId}, {text, Text}] ->
            UserId = e_chat_server_common:normalize_id(RawUserId),
            RoomId = e_chat_server_common:normalize_id(RawRoomId),
            Template = "SELECT * FROM messages WHERE user_id = '~w' AND room_id = '~w' AND text = '~s' ORDER BY created_at LIMIT 50",
            find_with_template(Template, [UserId, RoomId, Text]);
        [{user_id, RawUserId}, {room_id, RawRoomId}] ->
            UserId = e_chat_server_common:normalize_id(RawUserId),
            RoomId = e_chat_server_common:normalize_id(RawRoomId),
            Template =  "SELECT * FROM messages WHERE user_id = '~w' AND room_id = '~w' ORDER BY created_at LIMIT 50",
            find_with_template(Template, [UserId, RoomId]);
        [{room_id, RawRoomId}] ->
            RoomId = e_chat_server_common:normalize_id(RawRoomId),
            Template =  "SELECT * FROM messages WHERE room_id = '~w' ORDER BY created_at LIMIT 50",
            find_with_template(Template, [RoomId]);
        _ ->
            undefined
    end.

is_exists(Data) ->
    case find(Data) of
        undefined -> false;
        _ -> true
    end.

render(Message) ->
    [{<<"text">>, Message#message.text},
     {<<"user_id">>, Message#message.user_id},
     {<<"room_id">>, Message#message.room_id},
     {<<"created_at">>, e_chat_server_common:iso8601(Message#message.created_at)}].

%%%% Private functions
find_with_template(Template, Values) ->
    Request = e_chat_server_common:format(Template, Values),
    {ok, _, Records} = pgapp:equery(pgpool, Request, []),
    [#message{id = Id,
              room_id = RoomId,
              user_id = UserId,
              text = Text,
              state = State,
              created_at = CreatedAt}
        || {Id, RoomId, UserId, Text, State, CreatedAt} <- Records].
