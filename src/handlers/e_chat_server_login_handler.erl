-module(e_chat_server_login_handler).

-export([init/3]).
-export([known_methods/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([to_json/2]).
-export([perform/2]).

%%%% Standard callbacks
init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

known_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}, {<<"application/json; charset=utf-8">>, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, perform}, {<<"application/json; charset=utf-8">>, perform}], Req, State}.

%%%% Custom callbacks
to_json(Req, State) ->
    {true, Req, State}.

perform(Req, State) ->
    {ok, Data, Req2} = cowboy_req:body(Req),
    Result = case jsx:is_json(Data) of
        true ->
            [{<<"login">>, Login}, {<<"password">>, Password}] = jsx:decode(Data),
            e_chat_server_login_service:perform(Login, Password);
        false ->
            invalid_json
    end,
    {Status, Body} = case Result of
        invalid_json ->
            {422, jsx:encode([{<<"error">>, [{<<"code">>, <<"invalid_json">>}]}])};
        wrong_credentials ->
            {422, jsx:encode([{<<"error">>, [{<<"code">>, <<"wrong_credentials">>}]}])};
        user_not_found ->
            {422, jsx:encode([{<<"error">>, [{<<"code">>, <<"user_not_found">>}]}])};
        _ ->
            {201, jsx:encode([{<<"session_id">>, Result}])}
    end,
    {ok, Reply} = cowboy_req:reply(Status, [], Body, Req2),
    {halt, Reply, State}.
