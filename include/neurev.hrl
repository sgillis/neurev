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
                , generation :: pos_integer()
                , cortex_id :: neurev:id()
                , activation_function :: atom()
                , input :: neurev:neuron_input()
                , output :: [neurev:id()]
                , recurrent_output_ids :: [neurev:id()]
                }).

-record(cortex, { id :: neurev:id()
                , agent_id :: neurev:id()
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

-record(agent, { id
               , generation
               , population_id
               , specie_id
               , cortex_id
               , fingerprint
               , constraint
               , evo_hist = []
               , fitness
               , innovation_factor
               , pattern = []
               }).

-record(specie, { id
                , population_id
                , fingerprint
                , constraint
                , agent_ids = []
                , champion_ids = []
                , avg_fitness
                , innovation_factor
                }).

-record(population, { id
                    , polis_id
                    , specie_ids = []
                    , morphologies = []
                    , innovation_factor
                    }).

-record(constraint, { morphology = []
                    , neural_activation_functions = []
                    }).
