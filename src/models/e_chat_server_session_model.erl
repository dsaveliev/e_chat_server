-module(e_chat_server_session_model).

-export([create/1]).
-export([find/1]).
-export([delete/1]).
-export([is_exists/1]).

-include("../e_chat_server_models.hrl").

create(Data) ->
    [{user_id, UserId}] = Data,
    Template = "INSERT INTO sessions (user_id) VALUES ('~w')",
    Request = e_chat_server_common:format(Template, [UserId]),
    {ok, 1} = pgapp:equery(pgpool, Request, []),
    find([{user_id, UserId}]).

find(Data) ->
    case Data of
        [{user_id, UserId}] ->
            Template = "SELECT * FROM sessions WHERE user_id = '~w' ORDER BY created_at LIMIT 1",
            find_with_template(Template, [UserId]);
        [{uuid, Uuid}] ->
            Template = "SELECT * FROM sessions WHERE uuid = '~s' ORDER BY created_at LIMIT 1",
            find_with_template(Template, [Uuid]);
        _ ->
            undefined
    end.

delete(Data) ->
    Request = case Data of
        [{user_id, UserId}] ->
            Template = "DELETE FROM sessions WHERE user_id = '~w'",
            e_chat_server_common:format(Template, [UserId]);
        [{uuid, Uuid}] ->
            Template = "DELETE FROM sessions WHERE uuid = '~s'",
            e_chat_server_common:format(Template, [Uuid]);
        _ ->
            undefined
    end,
    {ok, _} = pgapp:equery(pgpool, Request, []).

is_exists(Data) ->
    case find(Data) of
        undefined -> false;
        _ -> true
    end.

%%%% Private functions
find_with_template(Template, Values) ->
    Request = e_chat_server_common:format(Template, Values),
    case pgapp:equery(pgpool, Request, []) of
        {ok, _, [{Id, Uuid, UserId, CreatedAt}]} ->
            #session{
               id = Id,
               uuid = Uuid,
               user_id = UserId,
               created_at = CreatedAt
            };
        {ok, _, []} ->
            undefined
    end.
