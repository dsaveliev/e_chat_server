-module(e_chat_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(PORT, 8081).
-define(ACCEPTORS, 100).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/:v/login",               e_chat_server_login_handler, []},
            {"/:v/logout",              e_chat_server_logout_handler, []},
            {"/:v/register",            e_chat_server_register_handler, []},
            {"/:v/users",               e_chat_server_users_handler, []},
            {"/:v/info",                e_chat_server_info_handler, []},
            {"/:v/rooms",               e_chat_server_rooms_handler, []},
            {"/:v/rooms/:id" ,          e_chat_server_rooms_handler, []},
            {"/:v/rooms/:id/messages" , e_chat_server_rooms_messages_handler, []},
            {"/:v/rooms/:id/chat",      e_chat_server_chat_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, ?ACCEPTORS,
        [{port, ?PORT}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    e_chat_server_sup:start_link().

stop(_State) ->
    ok.
