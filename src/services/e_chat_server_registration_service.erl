-module(e_chat_server_registration_service).

-export([perform/2]).

-include("../e_chat_server_models.hrl").

perform(Login, Password) ->
    case e_chat_server_user_model:is_exists([{login, Login}]) of
        false ->
            User = e_chat_server_user_model:create([{login, Login}, {password, Password}]),
            Session = e_chat_server_session_model:create([{user_id, User#user.id}]),
            Session#session.uuid;
        true ->
            user_already_exists
    end.

