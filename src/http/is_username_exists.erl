-module(is_username_exists).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, _) ->
  {ok, Body, Req} = cowboy_req:read_body(Req0),

  JsonData = jsx:decode(Body, [return_maps]),
  Username = maps:get(<<"username">>, JsonData),

  Query = "select count(*) from users where username = ?",
  {ok, _, [[Count]]} = db_gen_server:prepared_query(Query, [Username]),

  IsUsernameExists = case Count of
                       1 ->
                         jsx:encode(#{status => true});
                       _ ->
                         jsx:encode(#{status => false})
                     end,

  Response = cowboy_req:reply(200, #{}, IsUsernameExists, Req),
  {ok, Response, Req}.
