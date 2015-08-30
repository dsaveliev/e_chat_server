-module(e_chat_server_registry).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    room_pids = []
}).

%% API.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({user_connected, RoomId, UserId, SocketPid}, State = #state{room_pids = RoomPids}) ->
    case is_authorized(RoomId, UserId) of
      true ->
        {UpdatedRoomPids, RoomPid} = add_room(RoomId, RoomPids, UserId),
        bind_room_with_socket(RoomPid, SocketPid),
        {noreply, State#state{room_pids = UpdatedRoomPids}};
      false ->
        {noreply, State}
    end;
% handle_cast({user_disconnected, RoomId, UserId, SocketPid, RoomPid}, State) ->
%     case is_authorized(RoomId, UserId) of
%       true -> unbind_room_with_socket(RoomPid, SocketPid)
%     end,
%     {noreply, State};
%TODO: Удалять комнату из registry только если в ней больше нет сокетов и она долгое время неактивна
% На первой итерации оставим пустые комнаты активными.
% handle_cast({delete_room, RoomId}, State = #state{room_pids = RoomPids}) ->
%     UpdatedRoomPids = delete_room(RoomId, RoomPids),
%     {noreply, State#state{room_pids = UpdatedRoomPids}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%% Private functions
is_authorized(RoomId, UserId) ->
    UserAuthorised = e_chat_server_user_room_model:is_exists([
        {room_id, RoomId},
        {user_id, UserId}]),
    case UserAuthorised of
      undefined -> false;
      _ -> true
    end.

add_room(RoomId, RoomPids, UserId) ->
    case [Pid || {room, Id, Pid} <- RoomPids, Id =:= RoomId] of
        [RoomPid] ->
            {RoomPids, RoomPid};
        [] ->
            {ok, RoomPid} = supervisor:start_child(e_chat_server_room_sup, [RoomId, UserId]),
            {[{room, RoomId, RoomPid} | RoomPids], RoomPid}
    end.

% delete_room(RoomId, RoomPids) ->
%     case [Pid || {room, Id, Pid} <- RoomPids, Id =:= RoomId] of
%         [RoomPid] ->
%             gen_server:call(RoomPid, stop),
%             [Pid || {room, Id, Pid} <- RoomPids, Id =/= RoomId];
%         [] ->
%             RoomPids
%     end.

bind_room_with_socket(RoomPid, SocketPid) ->
    gen_server:cast(RoomPid, {add_socket, SocketPid}),
    SocketPid ! {add_room, RoomPid}.

% unbind_room_with_socket(RoomPid, SocketPid) ->
%     gen_server:cast(RoomPid, {delete_socket, SocketPid}).
