-module(e_chat_server_index_rooms_service).

-export([perform/1]).

-include("../../e_chat_server_models.hrl").

perform(CurrentUser) ->
    RoomUsers = [CurrentUser],
    Rooms = e_chat_server_room_model:find_all([RoomUsers]),
    build_response(Rooms).

%% Private functions
build_response(Rooms) ->
    [build_room(Room) || Room <- Rooms].

build_room(Room) ->
    Users = e_chat_server_user_model:find_by_room(Room),
    [{<<"id">>, Room#room.id},
     {<<"users">>, [[{<<"id">>, User#user.id},
                     {<<"login">>, User#user.login}] || User <- Users]}].
