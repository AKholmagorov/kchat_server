-module(ws_func).
-export([create_chat/2, get_messages/2, send_message/2, get_users/1, get_chats/1]).

create_chat(DecodedJSON, State) ->
  ReceiverID = maps:get(<<"receiverID">>, DecodedJSON),

  Query1 = "INSERT INTO chats () values ()",
  ok = db_gen_server:exe_query(Query1),

  {ok, _, [[NewChatID]]} = db_gen_server:exe_query(<<"SELECT LAST_INSERT_ID()">>),
  io:format("New chat has created: ~p~n", [NewChatID]),

  Query2 = "INSERT INTO chat_participants (chatID, userID) VALUES(?, ?)",
  ok = db_gen_server:prepared_query(Query2, [NewChatID, State]),
  ok = db_gen_server:prepared_query(Query2, [NewChatID, ReceiverID]),

  {reply, {text, jsx:encode(#{res_type => <<"chat_created">>, chat => get_chat(NewChatID, State)})}, State}.

get_messages(ChatID, State) ->
  io:format("message_got~n"),
  Query = "SELECT * FROM messages WHERE chatID = ?",
  {ok, _, Result} = db_gen_server:prepared_query(Query, [ChatID]),

  Messages = lists:map(fun(Row) ->
    [MsgID, ChatID, SenderID, Date, Text] = Row,
    #{msgID => MsgID, chatID => ChatID,
      senderID => SenderID, date => Date, text => Text}
                       end, Result),

  {reply, {text, jsx:encode(#{res_type => <<"get_messages">>, chatID => ChatID, messages => Messages})}, State}.

send_message(DecodedJSON, State) ->
  ChatID = maps:get(<<"chatID">>, DecodedJSON),
  Text = maps:get(<<"text">>, DecodedJSON),
  Date = maps:get(<<"date">>, DecodedJSON),

  Query0 = "INSERT INTO messages (chatID, senderID, text, date)
                  VALUES(?, ?, ?, ?)",
  Query1 = "SELECT LAST_INSERT_ID()",

  ok = db_gen_server:prepared_query(Query0, [ChatID, State, Text, Date]),
  {ok, _, [[MessageID]]} = db_gen_server:exe_query(Query1),

  {reply, {text, jsx:encode(#{res_type => <<"messages_sent">>, msgID => MessageID})}, State}.

get_users(State) ->
  Query = "SELECT id, username, avatar, bio, isOnline, lastSeen FROM users",
  {ok, _, Rows} = db_gen_server:exe_query(Query),

  Users = lists:map(fun(Row) ->
    [Id, Username, Avatar, Bio, Online, LastSeen] = Row,
    #{id => Id, username => Username, avatar => Avatar,
      isOnline => Online, lastSeen => LastSeen, bio => Bio}
                    end, Rows),

  {reply, {text, jsx:encode(#{users => Users, res_type => <<"get_users">>})}, State}.

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
