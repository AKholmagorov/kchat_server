-module(sign_up).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req, State) ->
  {ok, Body, _} = cowboy_req:read_body(Req),

  Data = jsx:decode(Body, [return_maps]),

  Username = maps:get(<<"username">>, Data),
  Password = maps:get(<<"password">>, Data),
  Avatar = maps:get(<<"avatar">>, Data),

  Query = "INSERT INTO users (username, password, avatar) VALUES (?, ?, ?)",
  Status = case db_gen_server:prepared_query(Query, [Username, Password, Avatar]) of
             ok ->
               Query2 = "SELECT id FROM users WHERE username = ?",
               {ok, _, [[ID]]} = db_gen_server:prepared_query(Query2, [Username]),
               %% ntf online users about newbie
               users_manager_gs:ntf_about_new_user(ID),
               jsx:encode(#{status => true, jwt => my_jwt:generate_jwk(ID)});
             _ ->
               jsx:encode(#{status => false})
           end,

  Response = cowboy_req:reply(200, #{}, Status, Req),
  {ok, Response, State}.
