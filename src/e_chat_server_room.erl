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
    user_id,
    room_id,
    sockets = []
}).

%% API.

-spec start_link(integer(), integer()) -> {ok, pid()}.
start_link(RoomId, UserId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [RoomId, UserId], []).

%% gen_server.

init([RoomId, UserId]) ->
    {ok, #state{room_id = RoomId, user_id = UserId}}.

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%%% Пересылка сообщения
handle_cast({forward_message, Text, UserId, FromSocketPid},
            State = #state{room_id = RoomId, sockets = SocketPids}) ->
    Message = e_chat_server_message_model:create([{user_id, UserId}, {room_id, RoomId}, {text, Text}]),
    forward_message(Message, FromSocketPid, SocketPids),
    {noreply, State};
handle_cast({add_socket, SocketPid}, State = #state{sockets = SocketPids}) ->
    UpdatedSocketPids = add_socket(SocketPid, SocketPids),
    {noreply, State#state{sockets = UpdatedSocketPids}};
% handle_cast({delete_socket, SocketPid}, State = #state{sockets = SocketPids}) ->
%     UpdatedSocketPids = delete_socket(SocketPid, SocketPids),
%    {noreply, State#state{sockets = UpdatedSocketPids}};
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

% delete_socket(SocketPid, SocketPids) ->
%     case [Pid || Pid <- SocketPids, SocketPid =:= Pid] of
%         [_] -> [Pid || Pid <- SocketPids, SocketPid =/= Pid];
%         [] -> SocketPids
%     end.

forward_message(Message, FromSocketPid, SocketPids) ->
    [Pid ! {send_message, Message} || Pid <- SocketPids, Pid =/= FromSocketPid].
