-module(e_chat_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
        {room_registry,
            {e_chat_server_registry, start_link, []},
            permanent, 1000, supervisor, [e_chat_server_registry]},
        {room_server,
            {e_chat_server_room_sup, start_link, []},
            permanent, 1000, supervisor, [e_chat_server_room_sup]}
    ],
    {ok, {{one_for_one, 1, 5}, Procs}}.
