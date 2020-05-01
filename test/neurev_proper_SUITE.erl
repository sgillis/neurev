-module(neurev_proper_SUITE).

%% Test server callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ genotype_props/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").


%%--------------------------------------------------------------------
%% Common test callback functions
%%--------------------------------------------------------------------

all() ->
  [ genotype_props
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

genotype_props({init, Config}) ->
  Config;
genotype_props({'end', _Config}) ->
  ok;
genotype_props(_Config) ->
  ?assert(proper:quickcheck(
            prop_neurev:prop_genotype_unique_ids(),
            [{numtests, 10}])),
  ?assert(proper:quickcheck(
            prop_neurev:prop_neuron_count_matches_densities(),
            [{numtests, 10}])).

