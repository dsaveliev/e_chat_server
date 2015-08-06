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

    User = e_chat_server_auth_service:perform(SessionId),
    {Room, _Users} = e_chat_server_search_room_service:perform(RoomId, User),

    Req2 = cowboy_req:compact(Req),
    {ok, Req2, #state{room = Room, user = User}}.

websocket_handle({text, Data}, Req, State = #state{room_pid = RoomPid}) ->
    Message = fetch_message(Data),
    gen_server:cast(RoomPid, {forward_message, Message, self()}),
    {ok, Req, State};
websocket_handle({binary, _Data}, Req, State) ->
    {ok, Req, State};
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
    {reply, {text, message_to_json(Message)}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State = #state{room = Room, user = User, room_pid = RoomPid}) ->
    gen_server:cast(e_chat_server_registry, {user_disconnected, Room#room.id, User#user.id, self(), RoomPid}).

%%%% Private functions
fetch_message(Data) ->
    %TODO: Валидация
    [{<<"text">>, Message}] = jsx:decode(Data),
    binary_to_list(Message).

message_to_json(Message) ->
  Data = e_chat_server_message_model:render(Message),
  jsx:encode(Data).
