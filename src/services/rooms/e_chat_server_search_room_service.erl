-module(e_chat_server_search_room_service).

-export([perform/2]).

-include("../../e_chat_server_models.hrl").

perform(_RoomId, undefined) ->
    undefined;
perform(RoomId, User) ->
    Room = e_chat_server_room_model:find([{id, RoomId}]),
    case Room of
        undefined -> undefined;
        _ ->
            Users = e_chat_server_user_model:find_by_room(Room),
            case lists:member(User, Users) of
                true -> {Room, Users};
                false -> undefined
            end
    end.
