-record(sensor, { id              :: neurev:id()
                , cortex_id       :: undefined | neurev:id()
                , name            :: atom()
                , vector_length   :: non_neg_integer()
                , fanout_ids = [] :: [neurev:id()]
                , scape           :: neurev:scape()
                }).

-record(actuator, { id             :: neurev:id()
                  , cortex_id      :: undefined | neurev:id()
                  , name           :: atom()
                  , vector_length  :: non_neg_integer()
                  , fanin_ids = [] :: [neurev:id()]
                  , scape          :: neurev:scape()
                  }).

-record(neuron, { id :: neurev:id()
                , cortex_id :: neurev:id()
                , activation_function :: atom()
                , input :: neurev:neuron_input()
                , output :: [neurev:id()]
                }).

-record(cortex, { id :: neurev:id()
                , sensor_ids :: [neurev:id()]
                , actuator_ids :: [neurev:id()]
                , neuron_ids :: [neurev:id()]
                }).

-record(genotype, { id :: neurev:id()
                  , cortex :: #cortex{}
                  , sensor :: #sensor{}
                  , actuator :: #actuator{}
                  , neurons :: [#neuron{}]
                  }).

-record(morphology, { sensor_name :: atom()
                    , actuator_name :: atom()
                    , layer_densities :: [pos_integer()]
                    }).
