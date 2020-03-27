-module(neurev_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Test server callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ genotype_construction/1
        ]).


%%--------------------------------------------------------------------
%% Common test callback functions
%%--------------------------------------------------------------------

all() ->
  [ genotype_construction
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

genotype_construction({init, Config}) ->
  Config;
genotype_construction({'end', _Config}) ->
  ok;
genotype_construction(_Config) ->
  SensorName = rng,
  ActuatorName = pts,
  HiddenLayerDensities = [1, 2],
  Genotype = neurev_genotype:construct(
               SensorName, ActuatorName, HiddenLayerDensities),
  UpdatedGenotype = neurev_exoself:map(Genotype),
  ?assertEqual(Genotype, UpdatedGenotype).
