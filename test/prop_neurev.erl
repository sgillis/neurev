-module(prop_neurev).

-export([ prop_genotype_unique_ids/0
        , prop_genotype_unique_ids/1
        , prop_neuron_count_matches_densities/0
        , prop_neuron_count_matches_densities/1
        ]).

-include_lib("proper/include/proper.hrl").
-include("neurev.hrl").


%% -----------------------------------------------------------------------------

prop_genotype_unique_ids(doc) ->
  "All ids in a genotype are unique";
prop_genotype_unique_ids(opts) ->
  [{numtests, 100}].

prop_genotype_unique_ids() ->
  ?FORALL( Genotype
         , neurev_constructor:genotype()
         , genotype_unique_ids(Genotype)
         ).

genotype_unique_ids(#genotype{ id = Id
                             , cortex = Cortex
                             , sensor = Sensor
                             , actuator = Actuator
                             , neurons = Neurons
                             }) ->
  Ids = [ Id
        , Cortex#cortex.id
        , Sensor#sensor.id
        , Actuator#actuator.id
          | [Neuron#neuron.id || Neuron <- Neurons]
        ],
  lists:usort(Ids) =:= lists:sort(Ids).


%% -----------------------------------------------------------------------------

prop_neuron_count_matches_densities(doc) ->
  "The amount of neurons in the genotype is equal to the sum of the numbers in "
  "the layer densities of the morphology plus one (the output neuron).";
prop_neuron_count_matches_densities(opts) ->
  [{numtests, 100}].

prop_neuron_count_matches_densities() ->
  ?FORALL( MorphGenotype
         , neurev_constructor:morph_genotype()
         , neuron_count_matches_densities(MorphGenotype)
         ).

neuron_count_matches_densities({ #morphology{layer_densities = LayerDensities}
                               , #genotype{neurons = Neurons}
                               }) ->
  lists:sum(LayerDensities) + 1 =:= length(Neurons).
