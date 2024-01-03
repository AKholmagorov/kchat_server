-module(db_gen_server).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0, prepared_query/2, exe_query/1, handle_info/2]).

start() ->
  gen_server:start_link({local, db_gs}, ?MODULE, [], []).

prepared_query(Query, Params) ->
  gen_server:call(db_gs, {prepared_query, Query, Params}).

exe_query(Query) ->
  gen_server:call(db_gs, {exe_query, Query}).

% =============================================
%                     API
% =============================================

init([]) ->
  {ok, DB_Pid} = connect_to_db(),
  MonitorRef = erlang:monitor(process, DB_Pid),
  {ok, #{db_pid => DB_Pid, monitor_ref => MonitorRef}}.

handle_call({prepared_query, Query, Params}, _From, State) ->
  DB_Pid = maps:get(db_pid, State),
  io:format("is_process_alive: ~p~n", [erlang:is_process_alive(DB_Pid)]),
  case erlang:is_process_alive(DB_Pid) of
    true ->
      {ok, PQ} = mysql:prepare(DB_Pid, Query),
      Result = mysql:execute(DB_Pid, PQ, Params),
      {reply, Result, State};
    false ->
      io:format("create new db process"),
      {ok, NewDB_Pid} = connect_to_db(),
      NewState = #{db_pid => NewDB_Pid},
      {ok, PQ} = mysql:prepare(NewDB_Pid, Query),
      Result = mysql:execute(NewDB_Pid, PQ, Params),
      {reply, Result, NewState}
  end;

handle_call({exe_query, Query}, _From, State) ->
  DB_Pid = maps:get(db_pid, State),
  io:format("is_process_alive: ~p~n", [erlang:is_process_alive(DB_Pid)]),
  case erlang:is_process_alive(DB_Pid) of
    true ->
      Result = mysql:query(DB_Pid, Query),
      {reply, Result, State};
    false ->
      io:format("create new db process"),
      {ok, NewDB_Pid} = connect_to_db(),
      Result = mysql:query(NewDB_Pid, Query),
      {reply, Result, #{db_pid => NewDB_Pid}}
  end.

connect_to_db() ->
  mysql:start_link([{host, "localhost"}, {user, "root"}, {password, "root"}, {database, "k_db"}]).

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

handle_info({'DOWN', _MonitorRef, process, _Pid, Reason}, State) ->
  io:format("Database connection process has terminated. Reason: ~p~n", [Reason]),
  NewState = State#{monitor_ref => undefined},
  {noreply, NewState}.
