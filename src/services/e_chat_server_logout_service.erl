-module(e_chat_server_logout_service).

-export([perform/1]).

-include("../e_chat_server_models.hrl").

perform(SessionId) ->
    Session = e_chat_server_session_model:find([{uuid, SessionId}]),
    {ok, _} = e_chat_server_session_model:delete([{user_id, Session#session.user_id}]).
