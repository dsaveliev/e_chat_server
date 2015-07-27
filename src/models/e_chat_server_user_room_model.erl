-module(e_chat_server_user_room_model).

-export([create/1]).
-export([find/1]).
-export([find_all/1]).
-export([is_exists/1]).

-include("../e_chat_server_models.hrl").

create(Data) ->
    [{user_id, UserId}, {room_id, RoomId}] = Data,
    Template = "INSERT INTO users_rooms (user_id, room_id) VALUES ('~w', '~w')",
    Request = e_chat_server_common:format(Template, [UserId, RoomId]),
    {ok, 1} = pgapp:equery(pgpool, Request, []),
    find([{user_id, UserId}]).

find(Data) ->
    case find_all(Data) of
        [] -> undefined;
        UserRooms -> lists:last(UserRooms)
    end.

find_all(Data) ->
    case Data of
        [{user_id, UserId}] ->
            Template = "SELECT * FROM users_rooms WHERE user_id = '~w' ORDER BY created_at",
            find_with_template(Template, [UserId]);
        [{room_id, RoomId}] ->
            Template = "SELECT * FROM users_rooms WHERE room_id = '~w' ORDER BY created_at",
            find_with_template(Template, [RoomId]);
        [{room_id, RoomId}, {user_id, UserId}] ->
            Template = "SELECT * FROM users_rooms WHERE room_id = '~w' AND user_id = '~w' ORDER BY created_at",
            find_with_template(Template, [RoomId, UserId]);
        _ ->
            undefined
    end.

is_exists(Data) ->
    case find(Data) of
        undefined -> false;
        _ -> true
    end.

%%%% Private functions
find_with_template(Template, Values) ->
    Request = e_chat_server_common:format(Template, Values),
    {ok, _, Records} = pgapp:equery(pgpool, Request, []),
    [#user_room{user_id = UserId,
                room_id = RoomId}
        || {UserId, RoomId} <- Records].
