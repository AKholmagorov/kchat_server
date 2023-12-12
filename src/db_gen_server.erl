-module(db_gen_server).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0, prepared_query/2, exe_query/1]).

% =============================================
% API
% =============================================

start() ->
  gen_server:start_link({local, main}, ?MODULE, [], []).

prepared_query(Query, Params) ->
  gen_server:call(main, {prepared_query, Query, Params}).

exe_query(Query) ->
  gen_server:call(main, {exe_query, Query}).

% =============================================
% gen_server
% =============================================

init([]) ->
  {ok, DB_Pid} = mysql:start_link([{host, "localhost"}, {user, "root"}, {password, "root"}, {database, "k_db"}]),
  {ok, #{db_pid => DB_Pid}}.

handle_call({prepared_query, Query, Params}, _From, State) ->
  DB_Pid = maps:get(db_pid, State),
  {ok, PQ} = mysql:prepare(DB_Pid, Query),
  Result = mysql:execute(DB_Pid, PQ, Params),

  {reply, Result, State};

handle_call({exe_query, Query}, _From, State) ->
  DB_Pid = maps:get(db_pid, State),
  Result = mysql:query(DB_Pid, Query),

  {reply, Result, State}.

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).
