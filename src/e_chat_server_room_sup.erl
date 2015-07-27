-module(e_chat_server_room_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
        {room_server,
            {e_chat_server_room, start_link, []},
            temporary, 1000, supervisor, [e_chat_server_room]}],
    {ok, {{simple_one_for_one, 3, 60}, Procs}}.

