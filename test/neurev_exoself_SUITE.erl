-module(neurev_exoself_SUITE).

 %% Test server callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ exoself_loop/1
        , exoself_xor/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("neurev.hrl").

-define(DEFAULT_TIMEOUT, 5000).

%%--------------------------------------------------------------------
%% Common test callback functions
%%--------------------------------------------------------------------

all() ->
  [ exoself_loop
  , exoself_xor
  ].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(Case, Config) ->
  application:ensure_all_started(neurev),
  ?MODULE:Case({init, Config}).

end_per_testcase(Case, Config) ->
  application:stop(neurev),
  ?MODULE:Case({'end', Config}).


%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

exoself_loop({init, Config}) ->
  Genotype =
    neurev_genotype:construct(#morphology{ sensor_name = rng
                                         , actuator_name = pts
                                         , layer_densities = [1,2]
                                         }),
  [{genotype, Genotype} | Config];
exoself_loop({'end', _Config}) ->
  ok;
exoself_loop(Config) ->
  register(trainer, self()),
  Genotype = ?config(genotype, Config),
  ExoPid = spawn(neurev_exoself, prep, [test_genotype, Genotype]),
  Result = receive
             {ExoPid, HighestFitness, EvalAcc, CycleAcc, _} ->
               {HighestFitness, EvalAcc, CycleAcc}
           after
             5000 ->
               false
           end,
  ?assertMatch({1, 10, 11}, Result),
  ok.

exoself_xor({init, Config}) ->
  Genotype =
    neurev_genotype:construct(#morphology{ sensor_name = xor_mimic
                                         , actuator_name = xor_mimic
                                         , layer_densities = [1,2]
                                         }),
  [{genotype, Genotype} | Config];
exoself_xor({'end', _Config}) ->
  ok;
exoself_xor(Config) ->
  register(trainer, self()),
  Genotype = ?config(genotype, Config),
  ExoPid = spawn(neurev_exoself, prep, [test_genotype, Genotype]),
  Result = receive
             {ExoPid, HighestFitness, EvalAcc, CycleAcc, _} ->
               {HighestFitness, EvalAcc, CycleAcc}
           after
             5000 ->
               false
           end,
  ?assertMatch({_, _, _}, Result),
  ok.
