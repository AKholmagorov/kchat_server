-module(ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_handle/2, websocket_info/2, terminate/3, websocket_init/1]).

%% State contains current user ID

init(Req, State) ->
  Token = cowboy_req:match_qs([token], Req),
  case maps:get(token, Token, undefined) of
    JWT ->
      case jose_jwt:verify(my_jwt:load_jwk(), JWT) of
        {true, {jose_jwt, Claims}, _} ->
          ID = maps:get(<<"id">>, Claims),
          io:format("user was connected ~p~n", [self()]),
          {cowboy_websocket, Req, ID, #{idle_timeout => 6000000}};
        {false, _, _} ->
          {ok, Req, State}
      end;
    _ ->
      io:format("Invalid token"),
      {ok, Req, State}
  end.

websocket_init(State) ->
  users_manager_gs:make_user_online(State, self()),
  io:format("user is online ~p~n", [self()]),
  {ok, State}.

websocket_handle({text, JSON}, State) ->
  DecodedJSON = jsx:decode(JSON, [return_maps]),
  case maps:get(<<"req_type">>, DecodedJSON) of
    <<"get_users">> -> ws_func:get_users(State);
    <<"get_chats">> -> ws_func:get_chats(State);
    <<"create_chat">> -> ws_func:create_chat(DecodedJSON, State);
    <<"send_message">> -> ws_func:send_message(DecodedJSON, State);
    <<"get_messages">> ->
      ChatID = maps:get(<<"chatID">>, DecodedJSON),
      ws_func:get_messages(ChatID, State);
    _ -> {reply, {text, <<"Unknown req_type">>}, State}
  end.

websocket_info({user_status_updated, ID, NewStatus, UpdatedLastSeen}, State) ->
  {
    [{
      text,
      jsx:encode(#{
        id => ID,
        rep_type => <<"user_status_updated">>,
        newStatus => NewStatus,
        updatedLastSeen => UpdatedLastSeen})
    }],
    State
  }.

terminate(_Reason, _WebSocket, State) ->
  case is_number(State) of
    true -> users_manager_gs:make_user_offline(State),
      io:format("Session terminated ~p~n", [self()]),
      ok;
    _ -> ok
  end.
