-module(e_chat_server_search_service).

-export([perform/1]).

-include("../e_chat_server_models.hrl").

perform(Query) ->
    case Query of
        %%%% Полный список пользователей _не_ возвращаем.
        %%%% TODO: Возвращать только список пользователей из чатов.
        undefined -> [];
        _ -> find_users(Query)
    end.

%%%% Private functions
find_users(Query) ->
    Users = e_chat_server_user_model:find_all([{q, Query}]),
    [e_chat_server_user_model:render(User) || User <- Users].
