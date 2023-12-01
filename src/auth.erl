-module(auth).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, _State) ->
  {ok, Body, Req1} = cowboy_req:read_body(Req),

  Data = jsx:decode(Body, [return_maps]),

  Username = maps:get(<<"username">>, Data),
  Password = maps:get(<<"password">>, Data),

  Query = "SELECT id FROM users WHERE username = ? AND password = ?",

  IsUserExists =
    case db_gen_server:prepared_query(Query, [Username, Password]) of
      {ok, _, [[UserID]]} ->
        jsx:encode(#{status => true, jwt => jwt:generate_jwk(UserID)});
      _ ->
        jsx:encode(#{status => false})
    end,

  Response = cowboy_req:reply(200, #{}, IsUserExists, Req1),
  {ok, Response, _State}.
