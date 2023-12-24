-module(ws_func).
-export([create_chat/2, create_group/2]).
-export([get_messages/2, send_message/2, get_users/1, get_chats/1, mark_messages_as_read/2, get_groups/1]).
-export([change_profile_data/2]).

create_chat(DecodedJSON, State) ->
  ReceiverID = maps:get(<<"receiverID">>, DecodedJSON),

  Query0 = "INSERT INTO chats () values ()",
  ok = db_gen_server:exe_query(Query0),

  {ok, _, [[NewChatID]]} = db_gen_server:exe_query(<<"SELECT LAST_INSERT_ID()">>),

  Query1 = "INSERT INTO chat_participants (chatID, userID) VALUES(?, ?)",
  ok = db_gen_server:prepared_query(Query1, [NewChatID, State]),
  ok = db_gen_server:prepared_query(Query1, [NewChatID, ReceiverID]),

  NewChatInstance = get_chat(NewChatID, ReceiverID),
  ok = users_manager_gs:ntf_about_new_chat_if_online(ReceiverID, NewChatInstance),

  {reply, {text, jsx:encode(#{res_type => <<"chat_created">>, chat => get_chat(NewChatID, State)})}, State}.

create_group(DecodedJSON, State) ->
  GroupData = maps:get(<<"group">>, DecodedJSON),
  Name = maps:get(<<"name">>, GroupData),
  Avatar = maps:get(<<"avatar">>, GroupData),

  %% create chat for group
  Query0 = "INSERT INTO chats (isPersonal) values (0)",
  ok = db_gen_server:exe_query(Query0),
  {ok, _, [[GroupChatID]]} = db_gen_server:exe_query(<<"SELECT LAST_INSERT_ID()">>),

  %% create new group and connect with chat
  %% instigator is admin
  Query1 = "INSERT INTO group_chats (chatID, name, avatar, membersCount, adminID)
            values (?, ?, ?, ?, ?)",
  ok = db_gen_server:prepared_query(Query1, [GroupChatID, Name, Avatar, 1, State]),

  %% add system message
  Query2 = "INSERT INTO messages (chatID, senderID, date, text, isRead, isSystemMsg)
            VALUES (?, ?, ?, ?, ?, ?)",
  CurrentDate = erlang:system_time(seconds),
  ok = db_gen_server:prepared_query(Query2, [GroupChatID, State, CurrentDate, "group created", 1, 1]),

  %% add instigator to the group
  Query3 = "INSERT INTO chat_participants (chatID, userID) VALUES(?, ?)",
  ok = db_gen_server:prepared_query(Query3, [GroupChatID, State]),

  NewGroupInstance = #{chatID => GroupChatID,
    name => Name,
    avatar => Avatar,
    lastMsg => <<" ">>,
    unreadMsgCount => 0,
    lastActivity => 0},

  {reply, {text, jsx:encode(#{res_type => <<"group_invitation">>, chat => NewGroupInstance})}, State}.

get_messages(DecodedJSON, State) ->
  ChatID = maps:get(<<"chatID">>, DecodedJSON),

  %% mark fetched messages as read TODO: refactor on fun call
  Query0 = "UPDATE messages SET isRead = 1 WHERE senderID != ?",
  ok = db_gen_server:prepared_query(Query0, [State]),

  Query1 = "SELECT * FROM messages WHERE chatID = ?",
  {ok, _, Result} = db_gen_server:prepared_query(Query1, [ChatID]),

  Messages = lists:map(
    fun(Row) ->
      [MsgID, ChatID, SenderID, Date, Text, IsRead, IsSystemMsg] = Row,
      #{msgID => MsgID,
        chatID => ChatID,
        senderID => SenderID,
        date => Date,
        text => Text,
        isRead => IsRead,
        isSystemMsg => IsSystemMsg}
    end, Result),

  %% notify others that messages was read
  users_manager_gs:broadcast_msgs_have_read(ChatID, State),

  {reply, {text, jsx:encode(#{res_type => <<"get_messages">>, chatID => ChatID, messages => Messages})}, State}.

mark_messages_as_read(DecodedJSON, State) ->
  ChatID = map_get(<<"chatID">>, DecodedJSON),

  Query0 = "UPDATE messages SET isRead = 1 WHERE senderID != ? AND chatID = ?",
  ok = db_gen_server:prepared_query(Query0, [State, ChatID]),

  Query1 = "UPDATE chat_participants SET unreadMsgCount = 0 WHERE userID = ? AND chatID = ?",
  db_gen_server:prepared_query(Query1, [State, ChatID]),

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
           FROM users",
  {ok, _, Rows} = db_gen_server:exe_query(Query),

  Users = lists:map(
    fun(Row) ->
      [ID, Username, Avatar, Bio, Online, LastSeen] = Row,
      #{id => ID, username => Username, avatar => Avatar,
        isOnline => Online, lastSeen => LastSeen, bio => Bio}
    end, Rows),

  {reply, {text, jsx:encode(#{res_type => <<"get_users">>, users => Users, currentUserID => State})}, State}.

get_groups(State) ->
  Query0 = "SELECT chatID, name, avatar, membersCount, adminID
           FROM group_chats
           WHERE chatID IN
            (SELECT chatID FROM chat_participants WHERE userID = ?)",
  {ok, _, Rows} = db_gen_server:prepared_query(Query0, [State]),

  Query1 = "SELECT userID FROM chat_participants WHERE chatID = ?",

  Groups = lists:map(
    fun(Row) ->
      [ChatID, Name, Avatar, MembersCount, AdminID] = Row,
      {ok, _, [MembersID]} = db_gen_server:prepared_query(Query1, [ChatID]),
      io:format("PP: ~p~n", [MembersID]),
      #{chatID => ChatID, name => Name, avatar => Avatar, membersCount => MembersCount, adminID => AdminID, membersID => MembersID}
    end, Rows),

  {reply, {text, jsx:encode(#{res_type => <<"get_groups">>, groups => Groups})}, State}.

get_chats(State) ->
  %% Get chat info: opponent avatar, username, id; unreadMsgCount, lastMsg, chatID
  Query0 = "SELECT
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
              AND cp.userID != ? OR c.isPersonal = 0 AND cp.userID = ?",
  {ok, _, Result} = db_gen_server:prepared_query(Query0, [State, State, State, State]),

  Chats = case Result of
            [] -> null;
            _ -> lists:map(
              fun(Row) ->
                [ChatID, ReceiverID, IsPersonal, LastMsg, LastActivity, UnreadMsgCount] = Row,
                case IsPersonal of
                  1 -> #{chatID => ChatID,
                    receiverID => ReceiverID,
                    isPersonal => IsPersonal,
                    lastMsg => LastMsg,
                    unreadMsgCount => UnreadMsgCount,
                    lastActivity => LastActivity};
                  0 ->
                    Query1 = "SELECT name, avatar FROM group_chats WHERE chatID = ?",
                    {ok, _, [GroupData]} = db_gen_server:prepared_query(Query1, [ChatID]),
                    [GroupName, GroupAvatar] = GroupData,
                    #{chatID => ChatID,
                      name => GroupName,
                      avatar => GroupAvatar,
                      isPersonal => IsPersonal,
                      lastMsg => LastMsg,
                      unreadMsgCount => UnreadMsgCount,
                      lastActivity => LastActivity}
                end
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
    lastMsg => <<"The last message.">>,
    unreadMsgCount => UnreadMsgCount,
    lastActivity => LastActivity
  }.

change_profile_data(DecodedJSON, State) ->
  Data = maps:get(<<"data">>, DecodedJSON),
  DataType = maps:get(<<"data_type">>, DecodedJSON),

  Query = case DataType of
            <<"username">> -> "UPDATE users SET username = ? WHERE id = ?";
            <<"password">> -> "UPDATE users SET password = ? WHERE id = ?";
            <<"user_avatar">> -> "UPDATE users SET avatar = ? WHERE id = ?";
            <<"user_bio">> -> "UPDATE users SET bio = ? WHERE id = ?"
          end,

  ok = db_gen_server:prepared_query(Query, [Data, State]),

  {reply, {text, jsx:encode(#{userID => State, res_type => <<"info_has_changed">>})}, State}.
