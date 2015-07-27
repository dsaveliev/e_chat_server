-module(e_chat_server_auth_service).

-export([perform/1]).

-include("../e_chat_server_models.hrl").

perform(SessionId) ->
    case SessionId of
        undefined -> undefined;
        _ -> validate_session(SessionId)
    end.

%%%% Private functions
validate_session(SessionId) ->
    case e_chat_server_common:is_uuid(SessionId) of
        true ->
            fetch_current_user(SessionId);
        false ->
            undefined
    end.

fetch_current_user(SessionId) ->
    case e_chat_server_session_model:is_exists([{uuid, SessionId}]) of
        true ->
            Session = e_chat_server_session_model:find([{uuid, SessionId}]),
            e_chat_server_user_model:find([{id, Session#session.user_id}]);
        false ->
            undefined
    end.
