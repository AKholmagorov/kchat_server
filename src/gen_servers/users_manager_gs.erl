-module(users_manager_gs).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0]).
-export([make_user_online/2, make_user_offline/1, make_users_offline_after_shutdown/0]).
-export([ntf_about_new_chat_if_online/2, ntf_about_new_message_if_online/2, broadcast_msgs_have_read/2, ntf_about_new_group_member/2]).
-export([ntf_about_group_invitation/3, ntf_about_member_kick/3, remove_group_ntf/1]).
-export([ntf_about_new_user/1]).

%% State contains map of online users #{ID => Pid}

start() ->
  gen_server:start_link({local, um_gs}, ?MODULE, [], []).

make_user_online(ID, Pid) ->
  gen_server:call(um_gs, {make_user_online, ID, Pid}),
  gen_server:cast(um_gs, {broadcast_online_status, ID, true}).

make_user_offline(ID) ->
  UpdatedLastSeen = gen_server:call(um_gs, {make_user_offline, ID}),
  gen_server:cast(um_gs, {broadcast_offline_status, ID, false, UpdatedLastSeen}).

%% after server crash online users didn't change their status,
%% this function make all users offline on server start up.
make_users_offline_after_shutdown() ->
  ok = gen_server:call(um_gs, {make_users_offline_after_shutdown}).

ntf_about_new_chat_if_online(ReceiverID, NewChatInstance) ->
  ok = gen_server:cast(um_gs, {ntf_about_new_chat_if_online, ReceiverID, NewChatInstance}).

ntf_about_new_message_if_online(ChatID, Msg) ->
  ok = gen_server:cast(um_gs, {ntf_about_new_message_if_online, ChatID, Msg}).

broadcast_msgs_have_read(ChatID, BroadcastInstigatorID) ->
  ok = gen_server:cast(um_gs, {broadcast_msgs_have_read, ChatID, BroadcastInstigatorID}).

ntf_about_new_group_member(ChatID, NewMemberID) ->
  ok = gen_server:cast(um_gs, {ntf_about_new_group_member, ChatID, NewMemberID}).

ntf_about_group_invitation(NewChatInstance, NewGroupInstance, NewMemberID) ->
  ok = gen_server:cast(um_gs, {ntf_about_group_invitation, NewChatInstance, NewGroupInstance, NewMemberID}).

ntf_about_member_kick(ChatID, KickedMemberID, IsForceKick) ->
  ok = gen_server:call(um_gs, {ntf_about_member_kick, ChatID, KickedMemberID, IsForceKick}).

ntf_about_new_user(NewUserID) ->
  ok = gen_server:call(um_gs, {ntf_about_new_user, NewUserID}).

remove_group_ntf(ChatID) ->
  ok = gen_server:call(um_gs, {remove_group_ntf, ChatID}).

% =============================================
%                     API
% =============================================

init([]) ->
  {ok, maps:new()}.

handle_call({make_user_online, ID, Pid}, _From, State) ->
  Query = "UPDATE users SET isOnline = 1 WHERE id = ?",
  ok = db_gen_server:prepared_query(Query, [ID]),

  NewState = maps:put(ID, Pid, State),
  {reply, ok, NewState};

handle_call({make_user_offline, ID}, _From, State) ->
  Query = "UPDATE users SET isOnline = 0 WHERE id = ?",
  ok = db_gen_server:prepared_query(Query, [ID]),

  Query2 = "UPDATE users SET lastSeen = ? WHERE id = ?",
  UpdatedLastSeen = erlang:system_time(seconds),
  ok = db_gen_server:prepared_query(Query2, [UpdatedLastSeen, ID]),

  NewState = maps:remove(ID, State),
  {reply, UpdatedLastSeen, NewState};

handle_call({make_users_offline_after_shutdown}, _From, State) ->
  Query = "UPDATE users SET isOnline = 0",
  ok = db_gen_server:exe_query(Query),

  {reply, ok, State};

handle_call({ntf_about_new_user, NewUserID}, _From, State) ->
  OnlineUsersID = maps:keys(State),
  OnlineUsersIDWithoutNew = lists:delete(NewUserID, OnlineUsersID),
  %% OnlineUsersIDWithoutNew = maps:remove(NewUserID, OnlineUsersID),

  Query0 = "SELECT * FROM users WHERE id = ?",
  {ok, _, [Result]} = db_gen_server:prepared_query(Query0, [NewUserID]),

  [_, Username, _, Avatar, Bio, IsOnline, LastSeen] = Result,

  User = #{
    id => NewUserID,
    username => Username,
    avatar => Avatar,
    isOnline => IsOnline,
    lastSeen => LastSeen,
    bio => Bio},

  lists:foreach(
    fun(UserID) ->
      Pid = maps:get(UserID, State),
      Pid ! {ntf_about_new_user, User}
    end, OnlineUsersIDWithoutNew),

  {reply, ok, State};

handle_call({remove_group_ntf, ChatID}, _From, State) ->
  OnlineUsersID = maps:keys(State),

  Query0 = "SELECT COUNT(*) FROM chat_participants WHERE chatID = ? AND userID = ?",

  lists:foreach(
    fun(UserID) ->
      case db_gen_server:prepared_query(Query0, [ChatID, UserID]) of
        {ok, _, [[1]]} ->
          Pid = maps:get(UserID, State),
          Pid ! {ntf_about_member_kick, ChatID, UserID, true};
        _ -> ok
      end
    end, OnlineUsersID),

  {reply, ok, State};

handle_call({ntf_about_member_kick, ChatID, KickedMemberID, IsForceKick}, _From, State) ->
  OnlineUsersID = maps:keys(State),

  %% select online users who is a member of the group
  Query0 = "SELECT COUNT(*) FROM chat_participants WHERE chatID = ? AND userID = ?",

  lists:foreach(
    fun(UserID) ->
      case db_gen_server:prepared_query(Query0, [ChatID, UserID]) of
        {ok, _, [[1]]} ->
          Pid = maps:get(UserID, State),
          Pid ! {ntf_about_member_kick, ChatID, KickedMemberID, IsForceKick};
        _ -> ok
      end
    end, OnlineUsersID),

  {reply, ok, State}.

handle_cast({ntf_about_new_chat_if_online, ReceiverID, NewChatInstance}, State) ->
  %% check if receiver is online
  case maps:get(ReceiverID, State, undefined) of
    undefined ->
      ok;
    ReceiverPID ->
      ReceiverPID ! {chat_invitation, NewChatInstance}
  end,
  {noreply, State};

handle_cast({ntf_about_new_message_if_online, ChatID, Msg}, State) ->
  OnlineUsersID = maps:keys(State),
  SenderID = maps:get(<<"senderID">>, Msg),

  Query = "SELECT COUNT(*) FROM chat_participants WHERE chatID = ? AND userID = ? AND userID != ?",

  lists:foreach(fun(UserID) ->
    case db_gen_server:prepared_query(Query, [ChatID, UserID, SenderID]) of
      {ok, _, [[1]]} ->
        Pid = maps:get(UserID, State),
        Pid ! {ntf_about_new_message_if_online, Msg};
      _ -> ok
    end
                end, OnlineUsersID),
  {noreply, State};

handle_cast({broadcast_online_status, UpdatedUserID, NewStatus}, State) ->
  maps:fold(
    fun(_ID, Pid, _Acc) ->
      Pid ! {user_status_updated, UpdatedUserID, NewStatus, null}
    end, 0, State),
  {noreply, State};

handle_cast({broadcast_offline_status, UpdatedUserID, NewStatus, UpdatedLastSeen}, State) ->
  maps:fold(fun(_ID, Pid, _Acc) ->
    Pid ! {user_status_updated, UpdatedUserID, NewStatus, UpdatedLastSeen} end, 0, State),
  {noreply, State};

handle_cast({broadcast_msgs_have_read, ChatID, BroadcastInstigatorID}, State) ->
  OnlineUsersID = maps:keys(State),
  Query = "SELECT COUNT(*) FROM chat_participants WHERE chatID = ? AND userID = ? AND userID != ?",

  %% send ntf that messages was read except receiver
  lists:foreach(
    fun(UserID) ->
      case db_gen_server:prepared_query(Query, [ChatID, UserID, BroadcastInstigatorID]) of
        {ok, _, [[1]]} ->
          Pid = maps:get(UserID, State),
          Pid ! {broadcast_msgs_have_read, ChatID};
        _ -> ok
      end
    end, OnlineUsersID),

  {noreply, State};

handle_cast({ntf_about_new_group_member, ChatID, NewMemberID}, State) ->
  OnlineUsersID = maps:keys(State),

  %% select online users who is a member of the group
  Query = "SELECT COUNT(*)
           FROM chat_participants
           WHERE chatID = ? AND userID = ? AND userID != ?",

  lists:foreach(
    fun(UserID) ->
      case db_gen_server:prepared_query(Query, [ChatID, UserID, NewMemberID]) of
        {ok, _, [[1]]} ->
          Pid = maps:get(UserID, State),
          Pid ! {ntf_about_new_group_member, ChatID, NewMemberID};
        _ -> ok
      end
    end, OnlineUsersID),

  {noreply, State};

handle_cast({ntf_about_group_invitation, NewChatInstance, NewGroupInstance, NewMemberID}, State) ->
  %% ntf user about group invitation if online
  Pid = maps:get(NewMemberID, State, undefined),
  case Pid of
    undefined -> ok;
    _ -> Pid ! {ntf_about_group_invitation, NewChatInstance, NewGroupInstance}
  end,

  {noreply, State}.
