-module(ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_handle/2, websocket_info/2, terminate/3, websocket_init/1]).

init(Req, State) ->
  io:format("Pid_init: ~p~n", [self()]),
  Token = cowboy_req:match_qs(['token'], Req),
  case maps:get('token', Token, undefined) of
    JWT when is_binary(JWT) ->
      case jose_jwt:verify(my_jwt:load_jwk(), JWT) of
        {true, {jose_jwt, Claims}, _} ->
          ID = maps:get(<<"id">>, Claims),
          io:format("user was connected ~p~n", [self()]),
          {cowboy_websocket, Req, ID, #{idle_timeout => 6000000}};
        {false, _, _} ->
          io:format("user was disconnected ~p~n", [self()]),
          {ok, Req, State}
      end;
    _ ->
      io:format("JWT fail binary check~n"),
      {ok, Req, State}
  end.

websocket_init(State) ->
  users_manager_gs:make_user_online(State, self()),
  io:format("user is online ~p~n", [self()]),
  {ok, State}.

websocket_handle({text, <<"get_users">>}, State) ->
  Query = "SELECT id, username, avatar, bio, isOnline, lastSeen FROM users",
  {ok, _, Rows} = db_gen_server:exe_query(Query),

  Users = lists:map(fun(Row) ->
    [Id, Username, Avatar, Bio, Online, LastSeen] = Row,
    #{id => Id, username => Username, avatar => Avatar,
      isOnline => Online, lastSeen => LastSeen, bio => Bio}
                    end, Rows),

  {reply, {text, jsx:encode(#{users => Users, rep_type => <<"get_users">>})}, State}.

websocket_info({user_status_updated, ID, NewStatus, UpdatedLastSeen}, State) ->
  {
    [
      {text, jsx:encode(#{id => ID,
          rep_type => <<"user_status_updated">>,
          newStatus => NewStatus,
          updatedLastSeen => UpdatedLastSeen})
      }
    ],
    State
  }.

terminate(_Reason, _WebSocket, State) ->
  case is_number(State) of
    true -> users_manager_gs:make_user_offline(State),
      io:format("Session terminated ~p~n", [self()]),
      ok;
    _ -> ok
  end.
