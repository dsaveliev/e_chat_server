-module(e_chat_server_create_room_service).

-export([perform/2]).

-include("../../e_chat_server_models.hrl").

perform(CurrentUser, Users) ->
    RoomUsers = lists:usort([CurrentUser | Users]),
    Rooms = e_chat_server_room_model:find_all([RoomUsers]),
    RoomExists = lists:any(fun (Room) -> users_in_room(Room, RoomUsers) end, Rooms),

    Room = case RoomExists of
        false ->
            NewRoom = e_chat_server_room_model:create([{owner_id, CurrentUser#user.id}]),
            bind_users(NewRoom, RoomUsers),
            NewRoom;
        true ->
            lists:last(Rooms)
            % e_chat_server_room_model:find([RoomUsers])
    end,
    build_response(Room, RoomUsers).

%% Private functions
users_in_room(Room, Users) ->
  lists:all(fun (User) -> room_has_user(Room, User) end, Users).

room_has_user(Room, User) ->
    e_chat_server_user_room_model:is_exists([{room_id, Room#room.id}, {user_id, User#user.id}]).

build_response(Room, Users) ->
     e_chat_server_room_model:render(Room, Users).

bind_users(Room, Users) ->
    [e_chat_server_user_room_model:create([{user_id, User#user.id},
                                           {room_id, Room#room.id}]) || User <- Users].
