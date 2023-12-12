-module(api_gateway_server).
-export([start/0]).

start() ->
  db_gen_server:start(),

  Dispatch = cowboy_router:compile([
    {'_', [
        {"/is_username_exists", is_username_exists, []},
        {"/sign_up", sign_up, []},
        {"/auth", auth, []},
        {"/ws", ws_handler, []}
    ]}
  ]),

  {ok, _} = cowboy:start_clear(my_http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}).
