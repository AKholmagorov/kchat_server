-module(main).
-export([start/0]).

start() ->
  db_gen_server:start(),
  users_manager_gs:start(),
  users_manager_gs:make_users_offline_after_shutdown(),

  Dispatch = cowboy_router:compile([
    {'_', [
        {"/is_username_exists", is_username_exists, []},
        {"/sign_up", sign_up, []},
        {"/auth", auth, []},
        {"/ws", ws_handler, []}
    ]}
  ]),

  {ok, _} = cowboy:start_clear(my_http_listener, [{port, 80}], #{env => #{dispatch => Dispatch}}).
