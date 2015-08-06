-module(e_chat_server_create_room_service).

-export([perform/2]).

-include("../../e_chat_server_models.hrl").

perform(CurrentUser, Users) ->
    RoomUsers = lists:usort([CurrentUser | Users]),
    Room = case e_chat_server_room_model:is_exists([RoomUsers]) of
        false ->
            NewRoom = e_chat_server_room_model:create([{owner_id, CurrentUser#user.id}]),
            bind_users(NewRoom, RoomUsers),
            NewRoom;
        true ->
            e_chat_server_room_model:find([RoomUsers])
    end,
    build_response(Room, RoomUsers).

%% Private functions
build_response(Room, Users) ->
     e_chat_server_room_model:render(Room, Users).

bind_users(Room, Users) ->
    [e_chat_server_user_room_model:create([{user_id, User#user.id},
                                           {room_id, Room#room.id}]) || User <- Users].
