-record(sensor, { id            :: neurev:id()
                , cortex_id     :: neurev:id()
                , name          :: neurev:modfun()
                , vector_length :: non_neg_integer()
                , fanout_ids    :: [neurev:id()]
                }).

-record(actuator, { id            :: neurev:id()
                  , cortex_id     :: neurev:id()
                  , name          :: neurev:modfun()
                  , vector_length :: non_neg_integer()
                  , fanin_ids     :: [neurev:id()]
                  }).

-record(neuron, { id :: neurev:id()
                , cortex_id :: neurev:id()
                , activation_function :: neurev:modfun()
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
