-module(ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, State) ->
  io:format("user was connected~n"),
  {cowboy_websocket, Req, State}.

websocket_handle({text, <<"get_users">>}, State) ->
  Query = "SELECT id, username, avatar, bio, isOnline, lastSeen FROM users",
  {ok, _, Rows} = db_gen_server:exe_query(Query),

  Users = lists:map(fun(Row) ->
    [Id, Username, Avatar, Bio, Online, LastSeen] = Row,
    #{id => Id, username => Username, avatar => Avatar,
      isOnline => Online, lastSeen => LastSeen, bio => Bio}
                    end, Rows),

  {reply, {text, jsx:encode(#{users => Users, rep_type => <<"get_users">>})}, State};

websocket_handle({text, <<"OPEN_CONNECTION">>}, State) ->
  {reply, {text, <<"OPENED">>}, State};

websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _WebSocket, _State) ->
  ok.
