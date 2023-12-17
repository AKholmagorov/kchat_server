-module(users_manager_gs).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0, make_user_online/2, make_user_offline/1, make_users_offline_after_shutdown/0]).

start() ->
  gen_server:start_link({local, um_gs}, ?MODULE, [], []).

make_user_online(ID, Pid) ->
  gen_server:call(um_gs, {make_user_online, ID, Pid}),
  gen_server:cast(um_gs, {broadcast_online_status, ID, true}).

make_user_offline(ID) ->
  UpdatedLastSeen = gen_server:call(um_gs, {make_user_offline, ID}),
  gen_server:cast(um_gs, {broadcast_offline_status, ID, false, UpdatedLastSeen}).

%% after server crash online users didn't change their status.
%% this function make all users offline on server start up.
make_users_offline_after_shutdown() ->
  ok = gen_server:call(um_gs, {make_users_offline_after_shutdown}).


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

  {reply, ok, State}.

handle_cast({broadcast_online_status, UpdatedUserID, NewStatus}, State) ->
  maps:fold(fun(_ID, Pid, _Acc) -> Pid ! {user_status_updated, UpdatedUserID, NewStatus, null} end, 0, State),
  {noreply, State};

handle_cast({broadcast_offline_status, UpdatedUserID, NewStatus, UpdatedLastSeen}, State) ->
  maps:fold(fun(_ID, Pid, _Acc) -> Pid ! {user_status_updated, UpdatedUserID, NewStatus, UpdatedLastSeen} end, 0, State),
  {noreply, State}.
