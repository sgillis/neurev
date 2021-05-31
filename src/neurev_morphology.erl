-module(neurev_morphology).

-export([ get_init_sensors/1
        , get_init_actuators/1
        ]).

-export([ xor_mimic/1 ]).

-include("neurev.hrl").

get_init_sensors(Morphology) ->
  Sensors = neurev_morphology:Morphology(sensors),
  [lists:nth(1, Sensors)].

get_init_actuators(Morphology) ->
  Actuators = neurev_morphology:Morphology(actuators),
  [lists:nth(1, Actuators)].

xor_mimic(sensors) ->
  [ #sensor{ name = xor_get_input
           , scape = {private, xor_sim}
           , vector_length = 2
           }
  ];
xor_mimic(actuators) ->
  [ #actuator{ name = xor_send_output
             , scape = {private, xor_sim}
             , vector_length = 1
             }
  ].
