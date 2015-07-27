-module(e_chat_server_chat_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-include("../e_chat_server_models.hrl").

-record(state, {
    room_id,
    session_id
}).

%%%% Standard callbacks
init(_, _, _) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
    self() ! post_init,

    {SessionId, _} = cowboy_req:qs_val(<<"session_id">>, Req, undefined),
    {RoomId, _} = cowboy_req:binding(id, Req, <<"0">>),

    Req2 = cowboy_req:compact(Req),
    {ok, Req2, #state{room_id = RoomId, session_id = SessionId}}.

websocket_handle({text, Data}, Req, State) ->
    {reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
    {reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
    {ok, Req, State}.

websocket_info(post_init, Req, State = #state{room_id = RoomId, session_id = SessionId}) ->
    User = find_user(SessionId),
    Room = find_room(RoomId, User),
    case Room of
        undefined ->
            {shutdown, Req, State};
        _ ->
            gen_server:cast(e_chat_server_registry, {user_connected, Room#room.id, User#user.id}),
            {ok, Req, State}
    end;
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State = #state{room_id = RoomId, session_id = SessionId}) ->
    User = find_user(SessionId),
    Room = find_room(RoomId, User),
    case Room of
        undefined ->
            ok;
        _ ->
            gen_server:cast(e_chat_server_registry, {user_connected, Room#room.id, User#user.id}),
            ok
    end.


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
