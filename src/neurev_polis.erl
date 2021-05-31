-module(neurev_polis).
-behaviour(gen_server).

-export([ start/0
        , start/1
        , stop/0
        , init/2
        , create/0
        , reset/0
        , status/0
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-include("neurev.hrl").
-include_lib("kernel/include/logger.hrl").

-record(state, { active_mods = []
               , active_scapes = []
               }).

-record(scape_summary, { address
                       , type
                       , parameters = []
                       }).

-define(MODS, []).
-define(PUBLIC_SCAPES, []).


start() ->
  case whereis(polis) of
    undefined ->
      gen_server:start(?MODULE, {?MODS, ?PUBLIC_SCAPES}, []);
    PolisPid ->
      ?LOG_INFO("Polis=~p is already running on this node", [PolisPid])
  end.

start(StartParams) ->
  gen_server:start(?MODULE, StartParams, []).

init(Pid, InitState) ->
  gen_server:cast(Pid, {init, InitState}).

stop() ->
  case whereis(polis) of
    undefined ->
      ?LOG_INFO("Polis can not be stopped, it is not online");
    PolisPid ->
      gen_server:cast(PolisPid, {stop, normal})
  end.

status() ->
  case whereis(polis) of
    undefined ->
      offline;
    Pid ->
      gen_server:call(Pid, status)
  end.

%% gen_server callbacks ========================================================

init({Mods, PublicScapes}) ->
  neurev_rand:seed(),
  process_flag(trap_exit, true),
  register(polis, self()),
  ?LOG_DEBUG("Parameters=~p", [{Mods, PublicScapes}]),
  create(),
  start_supmods(Mods),
  ActivePublicScapes = start_scapes(PublicScapes, []),
  ?LOG_INFO("Polis=~p is now online", [self()]),
  InitState = #state{ active_mods = Mods
                    , active_scapes = ActivePublicScapes
                    },
  {ok, InitState}.

handle_call({get_scape, Type}, {_CortexPid, _Ref}, S) ->
  ActivePublicScapes = S#state.active_scapes,
  ScapePid = case lists:keyfind(Type, 3, ActivePublicScapes) of
               false ->
                 undefined;
               PublicScape ->
                 PublicScape#scape_summary.address
             end,
  {reply, ScapePid, S};
handle_call(status, _From, S) ->
  {reply, online, S};
handle_call({stop, normal}, _From, S) ->
  {stop, normal, S};
handle_call({stop, shutdown}, _From, S) ->
  {stop, shutdown, S}.

handle_cast({init, InitState}, _S) ->
  {noreply, InitState};
handle_cast({stop, normal}, S) ->
  {stop, normal, S};
handle_cast({stop, shutdown}, S) ->
  {stop, shutdown, S}.

handle_info(_Info, S) ->
  {noreply, S}.

terminate(Reason, S) ->
  stop_supmods(S#state.active_mods),
  stop_scapes(S#state.active_scapes),
  application:stop(mnesia),
  ?LOG_INFO("Polis=~p is now offline reason=~p", [self(), Reason]),
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.


%% Internal functions ==========================================================

create() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  TableSpecs = [ {agent, record_info(fields, agent)}
               , {cortex, record_info(fields, cortex)}
               , {neuron, record_info(fields, neuron)}
               , {sensor, record_info(fields, sensor)}
               , {actuator, record_info(fields, actuator)}
               , {population, record_info(fields, population)}
               , {specie, record_info(fields, specie)}
               ],
  lists:foreach(fun create_table/1, TableSpecs).

create_table({TableName, Attrs}) ->
  mnesia:create_table(TableName, [ {disc_copies, [node()]}
                                 , {type, set}
                                 , {attributes, Attrs}
                                 ]).

reset() ->
  mnesia:stop(),
  ok = mnesia:delete_schema([node()]),
  polis:create().

start_supmods([ModName | ActiveMods]) ->
  ModName:start(),
  start_supmods(ActiveMods);
start_supmods([]) ->
  ok.

stop_supmods([ModName | ActiveMods]) ->
  ModName:stop(),
  stop_supmods(ActiveMods);
stop_supmods([]) ->
  ok.

start_scapes([S | Scapes], Acc) ->
  Type = S#scape_summary.type,
  Parameters = S#scape_summary.parameters,
  {ok, Pid} = neurev_scape:start_link({self(), Type, Parameters}),
  start_scapes(Scapes, [S#scape_summary{address = Pid} | Acc]);
start_scapes([], Acc) ->
  lists:reverse(Acc).

stop_scapes([S | Scapes]) ->
  Pid = S#scape_summary.address,
  gen_server:cast(Pid, {self(), stop, normal}),
  stop_scapes(Scapes);
stop_scapes([]) ->
  ok.
