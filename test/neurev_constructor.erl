-module(neurev_constructor).

-export([ genotype/0
        , morphology/0
        , morph_genotype/0
        ]).

-include_lib("neurev.hrl").
-include_lib("proper/include/proper.hrl").

genotype() ->
  ?LET(Morphology,
       morphology(),
       neurev_genotype:construct(Morphology)).

morphology() ->
  #morphology{ sensor_name = sensor_name()
             , actuator_name = actuator_name()
             , layer_densities = layer_densities()
             }.

morph_genotype() ->
  ?LET(Morphology,
       morphology(),
       {Morphology, neurev_genotype:construct(Morphology)}).

sensor_name() ->
  proper_types:oneof([ rng
                     , xor_mimic
                     ]).

actuator_name() ->
  proper_types:oneof([ pts
                     , xor_mimic
                     ]).

layer_densities() ->
  proper_types:list(proper_types:pos_integer()).
