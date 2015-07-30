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

handle_cast({forward_message, Message, FromSocketPid}, State = #state{sockets = SocketPids}) ->
    erlang:display(Message),
    erlang:display(FromSocketPid),
    %TODO: Сохранять Message в базе
    % Время в ISO 8601
    % Доработать формат сообщений {"message":"bla bla bla","send_at":"2012-04-23T18:25:43.511Z","user":{"id": 1,"login":"Василий"}}
    forward_message(Message, FromSocketPid, SocketPids),
    {noreply, State};
handle_cast({add_socket, SocketPid}, State = #state{sockets = SocketPids}) ->
    erlang:display(SocketPid),
    UpdatedSocketPids = add_socket(SocketPid, SocketPids),
    {noreply, State#state{sockets = UpdatedSocketPids}};
handle_cast({delete_socket, SocketPid}, State = #state{sockets = SocketPids}) ->
    erlang:display(SocketPid),
    UpdatedSocketPids = delete_socket(SocketPid, SocketPids),
    {noreply, State#state{sockets = UpdatedSocketPids}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%% Private function

add_socket(SocketPid, SocketPids) ->
    case [Pid || Pid <- SocketPids, SocketPid =:= Pid] of
        [_] -> SocketPids;
        [] -> [SocketPid | SocketPids]
    end.

delete_socket(SocketPid, SocketPids) ->
    case [Pid || Pid <- SocketPids, SocketPid =:= Pid] of
        [_] -> [Pid || Pid <- SocketPids, SocketPid =/= Pid];
        [] -> SocketPids
    end.

forward_message(Message, FromSocketPid, SocketPids) ->
    [Pid ! {send_message, Message} || Pid <- SocketPids, Pid =/= FromSocketPid].
