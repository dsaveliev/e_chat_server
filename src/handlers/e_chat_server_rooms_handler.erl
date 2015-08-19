-module(e_chat_server_rooms_handler).

-export([init/3]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([to_json/2]).
-export([perform/2]).

-record(state, {
    current_user
}).

%%%% Standard callbacks
init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

known_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

is_authorized(Req, State) ->
    {SessionId, Req2} = cowboy_req:header(<<"x-session-id">>, Req, undefined),
    User = e_chat_server_auth_service:perform(SessionId),
    case User of
        undefined ->
            Body = jsx:encode([{<<"error">>, [{<<"code">>, <<"unauthorized">>}]}]),
            {ok, Reply} = cowboy_req:reply(401, [], Body, Req2),
            {halt, Reply, State};
        _ ->
            {true, Req, #state{current_user = User}}
    end.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}, {<<"application/json; charset=utf-8">>, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, perform}, {<<"application/json; charset=utf-8">>, perform}], Req, State}.

%%%% Custom callbacks
to_json(Req, State = #state{current_user = CurrentUser}) ->
    {RoomId, _} = cowboy_req:binding(id, Req, undefined),
    Response = case RoomId of
        undefined ->
            e_chat_server_index_rooms_service:perform(CurrentUser);
        _ ->
            case e_chat_server_search_room_service:perform(RoomId, CurrentUser) of
                {Room, Users} ->
                    e_chat_server_room_model:render(Room, Users);
                undefined ->
                    [{<<"error">>, [{<<"code">>, <<"room_not_found">>}]}]
            end
    end,
    {jsx:encode(Response), Req, State}.

perform(Req, State = #state{current_user = CurrentUser}) ->
    {ok, Data, Req2} = cowboy_req:body(Req),
    Result = case jsx:is_json(Data) of
        true ->
            [{<<"users">>, UsersList}] = jsx:decode(Data),
            Users = e_chat_server_user_model:find_all([UsersList]),
            e_chat_server_create_room_service:perform(CurrentUser, Users);
        false ->
            invalid_json
    end,
    {Status, Body} = case Result of
        invalid_json ->
            {422, jsx:encode([{<<"error">>, [{<<"code">>, <<"invalid_json">>}]}])};
        _ ->
            {200, jsx:encode(Result)}
    end,
    {ok, Reply} = cowboy_req:reply(Status, [], Body, Req2),
    {halt, Reply, State}.
