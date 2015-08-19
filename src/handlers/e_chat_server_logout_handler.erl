-module(e_chat_server_logout_handler).

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
    {[<<"POST">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

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
to_json(Req, State) ->
    {true, Req, State}.

perform(Req, State) ->
    {SessionId, Req2} = cowboy_req:header(<<"x-session-id">>, Req),
    e_chat_server_logout_service:perform(SessionId),
    {ok, Reply} = cowboy_req:reply(201, [], jsx:encode([{}]), Req2),
    {halt, Reply, State}.
