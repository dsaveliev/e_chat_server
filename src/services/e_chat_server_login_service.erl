-module(e_chat_server_login_service).

-export([perform/2]).

-include("../e_chat_server_models.hrl").

perform(Login, Password) ->
    case e_chat_server_user_model:is_exists([{login, Login}]) of
        true ->
            User = e_chat_server_user_model:find([{login, Login}]),
            check_credentials(User, Password);
        false ->
            user_not_found
    end.

%%%% Private functions
check_credentials(User, Password) ->
    PasswordHash = e_chat_server_common:md5(Password),
    case User#user.password of
        PasswordHash ->
            e_chat_server_session_model:delete([{user_id, User#user.id}]),
            Session = e_chat_server_session_model:create([{user_id, User#user.id}]),
            Session#session.uuid;
        _ ->
            wrong_credentials
    end.

