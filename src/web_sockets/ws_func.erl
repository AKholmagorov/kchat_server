-module(ws_func).
-export([create_chat/2, get_messages/2, send_message/2, get_users/1, get_chats/1, get_current_user/1, mark_messages_as_read/2]).

create_chat(DecodedJSON, State) ->
  ReceiverID = maps:get(<<"receiverID">>, DecodedJSON),

  Query1 = "INSERT INTO chats () values ()",
  ok = db_gen_server:exe_query(Query1),

  {ok, _, [[NewChatID]]} = db_gen_server:exe_query(<<"SELECT LAST_INSERT_ID()">>),
  io:format("New chat has created: ~p~n", [NewChatID]),

  Query2 = "INSERT INTO chat_participants (chatID, userID) VALUES(?, ?)",
  ok = db_gen_server:prepared_query(Query2, [NewChatID, State]),
  ok = db_gen_server:prepared_query(Query2, [NewChatID, ReceiverID]),

  NewChatInstance = get_chat(NewChatID, ReceiverID),
  ok = users_manager_gs:ntf_about_new_chat_if_online(ReceiverID, NewChatInstance),

  {reply, {text, jsx:encode(#{res_type => <<"chat_created">>, chat => get_chat(NewChatID, State)})}, State}.

get_messages(ChatID, State) ->
  io:format("message_got~n"),

  %% mark fetched messages as read TODO: refactor on fun call
  Query = "UPDATE messages SET isRead = 1 WHERE senderID != ?",
  ok = db_gen_server:prepared_query(Query, [State]),

  Query3 = "SELECT * FROM messages WHERE chatID = ?",
  {ok, _, Result} = db_gen_server:prepared_query(Query3, [ChatID]),

  Messages = lists:map(fun(Row) ->
    [MsgID, ChatID, SenderID, Date, Text, IsRead] = Row,
    #{msgID => MsgID, chatID => ChatID,
      senderID => SenderID, date => Date, text => Text, isRead => IsRead}
                       end, Result),

  %% notify others that messages was read
  users_manager_gs:broadcast_msgs_have_read(ChatID, State),

  {reply, {text, jsx:encode(#{res_type => <<"get_messages">>, chatID => ChatID, messages => Messages})}, State}.

mark_messages_as_read(DecodedJSON, State) ->
  ChatID = map_get(<<"chatID">>, DecodedJSON),

  Query = "UPDATE messages SET isRead = 1 WHERE senderID != ?",
  ok = db_gen_server:prepared_query(Query, [State]),
  users_manager_gs:broadcast_msgs_have_read(ChatID, State),

  {reply, {text, jsx:encode(#{res_type => <<"messages_marked_read">>, chatID => ChatID})}, State}.

send_message(DecodedJSON, State) ->
  Msg = maps:get(<<"msg">>, DecodedJSON),
  ChatID = maps:get(<<"chatID">>, Msg),
  Text = maps:get(<<"text">>, Msg),
  Date = maps:get(<<"date">>, Msg),

  Query0 = "INSERT INTO messages (chatID, senderID, text, date)
            VALUES(?, ?, ?, ?)",
  Query1 = "SELECT LAST_INSERT_ID()",

  ok = db_gen_server:prepared_query(Query0, [ChatID, State, Text, Date]),
  {ok, _, [[MessageID]]} = db_gen_server:exe_query(Query1),

  %% send online user(-s) new message from chat
  users_manager_gs:ntf_about_new_message_if_online(ChatID, Msg),

  {reply, {text, jsx:encode(#{res_type => <<"message_sent">>, msgID => MessageID})}, State}.


get_users(State) ->
  Query = "SELECT id, username, avatar, bio, isOnline, lastSeen
           FROM users
           WHERE id != ?",
  {ok, _, Rows} = db_gen_server:prepared_query(Query, [State]),

  Users = lists:map(fun(Row) ->
    [Id, Username, Avatar, Bio, Online, LastSeen] = Row,
    #{id => Id, username => Username, avatar => Avatar,
      isOnline => Online, lastSeen => LastSeen, bio => Bio}
                    end, Rows),

  {reply, {text, jsx:encode(#{users => Users, res_type => <<"get_users">>})}, State}.

get_current_user(State) ->
  Query = "SELECT id, username, avatar, bio, isOnline, lastSeen
           FROM users
           WHERE id = ?",
  {ok, _, [Result]} = db_gen_server:prepared_query(Query, [State]),

  [Id, Username, Avatar, Bio, Online, LastSeen] = Result,
  User = #{id => Id, username => Username, avatar => Avatar,
    isOnline => Online, lastSeen => LastSeen, bio => Bio},

  {reply, {text, jsx:encode(#{user => User, res_type => <<"get_current_user">>})}, State}.

get_chats(State) ->
  Query = "SELECT chat_participants.chatID, chat_participants.userID, chats.isPersonal
           FROM chat_participants
           JOIN chats ON chat_participants.chatID = chats.id
           WHERE chat_participants.chatID IN
           (SELECT chatID FROM chat_participants WHERE userID = ?)
           AND chat_participants.userID != ?;",
  {ok, _, Result} = db_gen_server:prepared_query(Query, [State, State]),

  Chats = case Result of
            [] -> null;
            _ -> lists:map(fun(Row) ->
              [ChatID, ReceiverID, IsPersonal] = Row,
              #{chatID => ChatID, receiverID => ReceiverID,
                isPersonal => IsPersonal, lastMessage => <<"The last message.">>}
                           end, Result)
          end,

  {reply, {text, jsx:encode(#{chats => Chats, res_type => <<"get_chats">>})}, State}.

get_chat(ChatID, State) ->
  Query = "SELECT chat_participants.chatID, chat_participants.userID, chats.isPersonal
           FROM chat_participants
           JOIN chats ON chat_participants.chatID = chats.id
           WHERE chats.id = ?
           AND chat_participants.userID != ?;",
  {ok, _, [Result]} = db_gen_server:prepared_query(Query, [ChatID, State]),

  [ChatID, ReceiverID, IsPersonal] = Result,

  #{
    chatID => ChatID,
    receiverID => ReceiverID,
    isPersonal => IsPersonal,
    lastMessage => <<"The last message.">>
  }.
