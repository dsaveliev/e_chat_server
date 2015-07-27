-module(e_chat_server_room).
-behaviour(gen_server).

%% API.
-export([start_link/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    sockets = []
}).

%% API.

-spec start_link(integer(), integer()) -> {ok, pid()}.
start_link(_RoomId, _UserId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
