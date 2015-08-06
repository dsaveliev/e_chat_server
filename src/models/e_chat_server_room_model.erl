-module(e_chat_server_room_model).

-export([create/1]).
-export([find/1]).
-export([find_all/1]).
-export([is_exists/1]).
-export([render/2]).

-include("../e_chat_server_models.hrl").

create(Data) ->
    [{owner_id, OwnerId}] = Data,
    Template = "INSERT INTO rooms (owner_id) VALUES ('~w')",
    Request = e_chat_server_common:format(Template, [OwnerId]),
    {ok, 1} = pgapp:equery(pgpool, Request, []),
    find([{owner_id, OwnerId}]).

find(Data) ->
    case find_all(Data) of
        [] -> undefined;
        Rooms -> lists:last(Rooms)
    end.

find_all(Data) ->
    case Data of
        [{id, RawId}] ->
            Id = e_chat_server_common:normalize_id(RawId),
            Template = "SELECT * FROM rooms WHERE id = '~w' ORDER BY created_at",
            find_with_template(Template, [Id]);
        [{owner_id, RawOwnerId}] ->
            OwnerId = e_chat_server_common:normalize_id(RawOwnerId),
            Template = "SELECT * FROM rooms WHERE owner_id = '~w' ORDER BY created_at",
            find_with_template(Template, [OwnerId]);
        [Users] ->
            IdsList = [integer_to_list(User#user.id) || User <- Users],
            Ids = string:join(IdsList, ","),
            Template = "SELECT rooms.* FROM rooms LEFT JOIN users_rooms ON (rooms.id = users_rooms.room_id) WHERE users_rooms.user_id IN (~s)",
            find_with_template(Template, [Ids]);
        _ ->
            undefined
    end.

is_exists(Data) ->
    case find(Data) of
        undefined -> false;
        _ -> true
    end.

render(Room, Users) ->
    [{<<"id">>, Room#room.id},
     {<<"users">>, [e_chat_server_user_model:render(User) || User <- Users]}].

%%%% Private functions
find_with_template(Template, Values) ->
    Request = e_chat_server_common:format(Template, Values),
    {ok, _, Records} = pgapp:equery(pgpool, Request, []),
    [#room{id = Id,
           owner_id = OwnerId,
           created_at = CreatedAt}
        || {Id, OwnerId, CreatedAt} <- Records].
