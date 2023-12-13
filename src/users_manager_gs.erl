-module(users_manager_gs).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0, make_user_online/2, make_user_offline/1]).

start() ->
  gen_server:start_link({local, um_gs}, ?MODULE, [], []).

make_user_online(ID, Pid) ->
  gen_server:call(um_gs, {make_user_online, ID, Pid}),
  gen_server:cast(um_gs, broadcast_new_status).

make_user_offline(ID) ->
  gen_server:call(um_gs, {make_user_offline, ID}),
  gen_server:cast(um_gs, broadcast_new_status).


% =============================================
%                     API
% =============================================

init([]) ->
  {ok, maps:new()}.

handle_call({make_user_online, ID, Pid}, _From, State) ->
  Query = "UPDATE users SET isOnline = 1 WHERE id = ?",
  io:format("ID_from_call: ~p~n", [ID]),
  ok = db_gen_server:prepared_query(Query, [ID]),

  NewState = maps:put(ID, Pid, State),
  {reply, ok, NewState};

handle_call({make_user_offline, ID}, _From, State) ->
  Query = "UPDATE users SET isOnline = 0 WHERE id = ?",
  ok = db_gen_server:prepared_query(Query, [ID]),

  Query2 = "UPDATE users SET lastSeen = ? WHERE id = ?",
  Timestamp = erlang:system_time(seconds),
  ok = db_gen_server:prepared_query(Query2, [Timestamp, ID]),

  NewState = maps:remove(ID, State),
  {reply, ok, NewState}.

handle_cast(broadcast_new_status, State) ->
  maps:fold(fun(ID, Pid, _Acc) -> Pid ! {user_status_updated, ID} end, 0, State),
  {noreply, State}.
