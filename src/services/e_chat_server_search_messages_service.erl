-module(e_chat_server_search_messages_service).

-export([perform/2]).

-include("../e_chat_server_models.hrl").

perform(RoomId, User) ->
    {Room, _Users} = e_chat_server_search_room_service:perform(RoomId, User),
    case Room of
        undefined ->
            [[]];
        _ ->
            Messages = e_chat_server_message_model:find_all([{room_id, RoomId}]),
            [e_chat_server_message_model:render(Message) || Message <- Messages]
    end.
