-module(neurev_cortex_SUITE).

 %% Test server callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ cortex_loop/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("neurev.hrl").

-define(DEFAULT_TIMEOUT, 5000).

%%--------------------------------------------------------------------
%% Common test callback functions
%%--------------------------------------------------------------------

all() ->
  [ cortex_loop
  ].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(Case, Config) ->
  ?MODULE:Case({init, Config}).

end_per_testcase(Case, Config) ->
  ?MODULE:Case({'end', Config}).


%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

cortex_loop({init, Config}) ->
  CortexPid = setup_cortex(),
  [{cortex_pid, CortexPid} | Config];
cortex_loop({'end', Config}) ->
  CortexPid = ?config(cortex_pid, Config),
  cleanup_cortex(CortexPid),
  ok;
cortex_loop(Config) ->
  CortexPid = ?config(cortex_pid, Config),
  _SensorPid = self(),
  ActuatorPid = self(),
  ?assertEqual({CortexPid, sync}, receive_with_timeout()),
  CortexPid ! {ActuatorPid, sync, 5.0, 0},
  CortexPid ! {ActuatorPid, sync, 4.0, 1},
  ?assertMatch({CortexPid, evaluation_completed, 9.0, 1, _},
               receive_with_timeout()),
  ok.

setup_cortex() ->
  CortexPid = neurev_cortex:gen(self(), node()),
  CortexPid ! { self()
              , "cortex_id"
              , [self()]
              , []
              , [self(), self()]
              },
  CortexPid.

cleanup_cortex(CortexPid) ->
  CortexPid ! {self(), terminate}.

receive_with_timeout() ->
  receive_with_timeout(?DEFAULT_TIMEOUT).

receive_with_timeout(Timeout) ->
  receive
    Msg ->
      Msg
  after
    Timeout ->
      {error, timeout}
  end.
