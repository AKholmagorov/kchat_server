-module(ws_func).
-export([create_chat/2, get_messages/2, send_message/2, get_users/1, get_chats/1, get_current_user/1, mark_messages_as_read/2]).

create_chat(DecodedJSON, State) ->
  ReceiverID = maps:get(<<"receiverID">>, DecodedJSON),

  Query1 = "INSERT INTO chats () values ()",
  ok = db_gen_server:exe_query(Query1),

  {ok, _, [[NewChatID]]} = db_gen_server:exe_query(<<"SELECT LAST_INSERT_ID()">>),

  Query2 = "INSERT INTO chat_participants (chatID, userID) VALUES(?, ?)",
  ok = db_gen_server:prepared_query(Query2, [NewChatID, State]),
  ok = db_gen_server:prepared_query(Query2, [NewChatID, ReceiverID]),

  NewChatInstance = get_chat(NewChatID, ReceiverID),
  ok = users_manager_gs:ntf_about_new_chat_if_online(ReceiverID, NewChatInstance),

  {reply, {text, jsx:encode(#{res_type => <<"chat_created">>, chat => get_chat(NewChatID, State)})}, State}.

get_messages(DecodedJSON, State) ->
  ChatID = maps:get(<<"chatID">>, DecodedJSON),

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

  Query = "UPDATE messages SET isRead = 1 WHERE senderID != ? AND chatID = ?",
  ok = db_gen_server:prepared_query(Query, [State, ChatID]),

  Query2 = "UPDATE chat_participants SET unreadMsgCount = 0 WHERE userID = ? AND chatID = ?",
  db_gen_server:prepared_query(Query2, [State, ChatID]),

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

  Query2 = "UPDATE chats SET lastMsg = ?, lastActivity = ? WHERE id = ?",
  Query3 = "UPDATE chat_participants
            SET unreadMsgCount = unreadMsgCount + 1
            WHERE userID != ? AND chatID = ?",
  ok = db_gen_server:prepared_query(Query2, [Text, Date, ChatID]),
  ok = db_gen_server:prepared_query(Query3, [State, ChatID]),

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
  %% Get chat info: opponent avatar, username, id; unreadMsgCount, lastMsg, chatID
  Query = "SELECT
              cp.chatID,
              cp.userID,
              c.isPersonal,
              c.lastMsg,
              c.lastActivity,
              (SELECT cp2.unreadMsgCount
               FROM chat_participants cp2
               WHERE cp2.chatID = cp.chatID AND cp2.userID = ?) AS unreadMsgCount
          FROM
              chat_participants cp
          JOIN
              chats c ON cp.chatID = c.id
          WHERE
              cp.chatID IN (SELECT chatID FROM chat_participants WHERE userID = ?)
              AND cp.userID != ?;",
  {ok, _, Result} = db_gen_server:prepared_query(Query, [State, State, State]),

  Chats = case Result of
            [] -> null;
            _ -> lists:map(fun(Row) ->
              [ChatID, ReceiverID, IsPersonal, LastMsg, LastActivity, UnreadMsgCount] = Row,
              #{chatID => ChatID,
                receiverID => ReceiverID,
                isPersonal => IsPersonal,
                lastMessage => LastMsg,
                unreadMsgCount => UnreadMsgCount,
                lastActivity => LastActivity}
                           end, Result)
          end,

  {reply, {text, jsx:encode(#{chats => Chats, res_type => <<"get_chats">>})}, State}.

get_chat(ChatID, State) ->
  Query = "SELECT chat_participants.chatID, chat_participants.userID, chats.isPersonal, chat_participants.unreadMsgCount, chats.lastActivity
           FROM chat_participants
           JOIN chats ON chat_participants.chatID = chats.id
           WHERE chats.id = ?
           AND chat_participants.userID != ?;",
  {ok, _, [Result]} = db_gen_server:prepared_query(Query, [ChatID, State]),

  [ChatID, ReceiverID, IsPersonal, UnreadMsgCount, LastActivity] = Result,

  #{
    chatID => ChatID,
    receiverID => ReceiverID,
    isPersonal => IsPersonal,
    lastMessage => <<"The last message.">>,
    unreadMsgCount => UnreadMsgCount,
    lastActivity => LastActivity
  }.
