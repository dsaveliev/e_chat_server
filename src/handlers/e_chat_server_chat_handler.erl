-module(e_chat_server_chat_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-include("../e_chat_server_models.hrl").

-record(state, {
    user,
    room,
    room_pid
}).

%%%% Standard callbacks
init(_, _, _) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
    self() ! post_init,

    {SessionId, _} = cowboy_req:qs_val(<<"session_id">>, Req, undefined),
    {RoomId, _} = cowboy_req:binding(id, Req, <<"0">>),

    User = find_user(SessionId),
    Room = find_room(RoomId, User),

    Req2 = cowboy_req:compact(Req),
    {ok, Req2, #state{room = Room, user = User}}.

websocket_handle({text, Data}, Req, State = #state{room_pid = RoomPid}) ->
    Message = fetch_message(Data),
    gen_server:cast(RoomPid, {forward_message, Message, self()}),
    {reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
    {reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
    {ok, Req, State}.

websocket_info(post_init, Req, State = #state{room = Room, user = User}) ->
    case Room of
        undefined ->
            {shutdown, Req, State};
        _ ->
            gen_server:cast(e_chat_server_registry, {user_connected, Room#room.id, User#user.id, self()}),
            {ok, Req, State}
    end;
websocket_info({add_room, RoomPid}, Req, State) ->
    {ok, Req, State#state{room_pid = RoomPid}};
websocket_info({send_message, Message}, Req, State) ->
    {reply, {text, Message}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State = #state{room = Room, user = User, room_pid = RoomPid}) ->
    gen_server:cast(e_chat_server_registry, {user_disconnected, Room#room.id, User#user.id, self(), RoomPid}).

%%%% Private functions
find_user(SessionId) ->
    e_chat_server_auth_service:perform(SessionId).

find_room(_RoomId, undefined) ->
    undefined;
find_room(RoomId, User) ->
    Id = binary_to_integer(RoomId),
    Room = e_chat_server_room_model:find([{id, Id}]),
    case Room of
        undefined -> undefined;
        _ ->
            Users = e_chat_server_user_model:find_by_room(Room),
            case lists:member(User, Users) of
                true -> Room;
                false -> undefined
            end
    end.

fetch_message(Data) ->
    %TODO: Валидация
    [{<<"message">>, Message}] = jsx:decode(Data),
    binary_to_list(Message).
