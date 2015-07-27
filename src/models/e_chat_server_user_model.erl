-module(e_chat_server_user_model).

-export([create/1]).
-export([find/1]).
-export([find_all/1]).
-export([find_by_room/1]).
-export([is_exists/1]).

-include("../e_chat_server_models.hrl").

create(Data) ->
    [{login, Login}, {password, Password}] = Data,
    PasswordHash = e_chat_server_common:md5(Password),
    Template = "INSERT INTO users (login, password) VALUES ('~s','~s')",
    Request = e_chat_server_common:format(Template, [binary_to_list(Login), PasswordHash]),
    {ok, 1} = pgapp:equery(pgpool, Request, []),
    find([{login, Login}]).

find(Data) ->
    case find_all(Data) of
        [] -> undefined;
        Users -> lists:last(Users)
    end.

find_all(Data) ->
    case Data of
        [{id, Id}] ->
            Template = "SELECT * FROM users WHERE id = '~w'",
            find_with_template(Template, [Id]);
        [{login, Login}] ->
            Template = "SELECT * FROM users WHERE login = '~s'",
            find_with_template(Template, [Login]);
        [{q, Query}] ->
            Template = "SELECT * FROM users WHERE login LIKE '%~s%'",
            find_with_template(Template, [Query]);
        [UserList] ->
            [find([{id, Id}]) || [{<<"id">>, Id}, {<<"login">>, _}] <- UserList];
        _ ->
            []
    end.

find_by_room(Room) ->
    Template = "SELECT DISTINCT users.* FROM users LEFT JOIN users_rooms ON (users_rooms.room_id = ~w)",
    find_with_template(Template, [Room#room.id]).

is_exists(Data) ->
    case find(Data) of
        undefined -> false;
        _ -> true
    end.

%%%% Private functions
find_with_template(Template, Values) ->
    Request = e_chat_server_common:format(Template, Values),
    {ok, _, Records} = pgapp:equery(pgpool, Request, []),
    [#user{id = Id,
           login = Login,
           password = Password,
           created_at = CreatedAt}
        || {Id, Login, Password, CreatedAt} <- Records].
