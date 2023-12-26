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
  {ok, State}.

websocket_handle({text, JSON}, State) ->
  DecodedJSON = jsx:decode(JSON, [return_maps]),
  case maps:get(<<"req_type">>, DecodedJSON) of
    <<"get_users">> -> ws_func:get_users(State);
    <<"get_chats">> -> ws_func:get_chats(State);
    <<"get_groups">> -> ws_func:get_groups(State);
    <<"create_chat">> -> ws_func:create_chat(DecodedJSON, State);
    <<"send_message">> -> ws_func:send_message(DecodedJSON, State);
    <<"mark_messages_as_read">> -> ws_func:mark_messages_as_read(DecodedJSON, State);
    <<"get_messages">> -> ws_func:get_messages(DecodedJSON, State);
    <<"create_group">> -> ws_func:create_group(DecodedJSON, State);
    <<"remove_group">> -> ws_func:remove_group(DecodedJSON, State);
    <<"add_user_to_group">> -> ws_func:add_user_to_group(DecodedJSON, State);
    <<"remove_user_from_group">> -> ws_func:remove_user_from_group(DecodedJSON, State);
    <<"change_profile_data">> -> ws_func:change_profile_data(DecodedJSON, State);
    _ -> {reply, {text, jsx:encode(#{res_type => <<"Unknown request">>})}, State}
  end.

websocket_info({user_status_updated, ID, NewStatus, UpdatedLastSeen}, State) ->
  {[{text, jsx:encode(
    #{id => ID,
      res_type => <<"user_status_updated">>,
      newStatus => NewStatus,
      updatedLastSeen => UpdatedLastSeen}
  )}], State};

websocket_info({chat_invitation, NewChatInstance}, State) ->
  {[{text, jsx:encode(#{res_type => <<"chat_created">>, chat => NewChatInstance})}], State};

websocket_info({ntf_about_new_message_if_online, Msg}, State) ->
  {[{text, jsx:encode(#{res_type => <<"new_message">>, msg => Msg})}], State};

websocket_info({ntf_about_new_group_member, ChatID, NewMemberID}, State) ->
  {[{text, jsx:encode(#{res_type => <<"new_group_member">>, chatID => ChatID, newMemberID => NewMemberID})}], State};

websocket_info({ntf_about_group_invitation, NewChatInstance, NewGroupInstance}, State) ->
  {[{text, jsx:encode(#{res_type => <<"group_invitation">>, chat => NewChatInstance, group => NewGroupInstance})}], State};

websocket_info({broadcast_msgs_have_read, ChatID}, State) ->
  {[{text, jsx:encode(#{res_type => <<"messages_read">>, chatID => ChatID})}], State};

websocket_info({ntf_about_member_kick, ChatID, KickedMemberID, IsForceKick}, State) ->
  {[{text, jsx:encode(#{res_type => <<"group_member_left">>, chatID => ChatID, userID => KickedMemberID, isForceKick => IsForceKick})}], State};

websocket_info({ntf_about_new_user, User}, State) ->
  {[{text, jsx:encode(#{res_type => <<"new_user_signed_up">>, user => User})}], State}.

terminate(_Reason, _WebSocket, State) ->
  case is_number(State) of
    true -> users_manager_gs:make_user_offline(State), ok;
    _ -> ok
  end.
