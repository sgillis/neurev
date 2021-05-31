-module(neurev_genotype).

-include("neurev.hrl").
-include_lib("kernel/include/logger.hrl").

-export([ clone_agent/2
        , construct_agent/3
        , delete_agent/1
        , delete_agent_safe/1
        , log/1
        , read/1
        , create_neural_weights/2
        ]).

construct_agent(SpecieId, AgentId, SpecConstraint) ->
  neurev_rand:seed(),
  Generation = 0,
  {CortexId, Pattern} = construct_cortex(AgentId, Generation, SpecConstraint),
  Agent = #agent{ id = AgentId
                , cortex_id = CortexId
                , specie_id = SpecieId
                , constraint = SpecConstraint
                , generation = Generation
                , pattern = Pattern
                , evo_hist = []
                },
  write(Agent),
  update_fingerprint(AgentId).

construct_cortex(AgentId, Generation, SpecConstraint) ->
  CortexId = {{origin, neurev_rand:generate_id()}, cortex},
  Morphology = SpecConstraint#constraint.morphology,
  Sensors = [S#sensor{ id = {{-1, neurev_rand:generate_id()}, sensor}
                     , cortex_id = CortexId }
             || S <- neurev_morphology:get_init_sensors(Morphology)],
  Actuators = [S#actuator{ id = {{-1, neurev_rand:generate_id()}, actuator}
                             , cortex_id = CortexId}
               || S <- neurev_morphology:get_init_actuators(Morphology)],
  NeuronIds = construct_initial_neuro_layer(
                CortexId, Generation,SpecConstraint, Sensors, Actuators,
                [], []),
  SensorIds = [S#sensor.id || S <- Sensors],
  ActuatorIds = [A#actuator.id || A <- Actuators],
  Cortex = #cortex{ id = CortexId
                  , agent_id = AgentId
                  , neuron_ids = NeuronIds
                  , sensor_ids = SensorIds
                  , actuator_ids = ActuatorIds
                  },
  write(Cortex),
  {CortexId, [{0, NeuronIds}]}.

construct_initial_neuro_layer(CortexId, Generation, SpecConstraint, Sensors,
                              [A | Actuators], ActuatorAcc, NeuronIdAcc) ->
  NeuronIds = [{{0, Id}, neuron}
               || Id <- generate_ids(A#actuator.vector_length, [])],
  USensors = construct_initial_neurons(
               CortexId, Generation, SpecConstraint, NeuronIds, Sensors, A),
  UActuator = A#actuator{fanin_ids = NeuronIds},
  construct_initial_neuro_layer(CortexId, Generation, SpecConstraint, USensors,
                                Actuators, [UActuator | ActuatorAcc],
                                lists:append(NeuronIds, NeuronIdAcc));
construct_initial_neuro_layer(_CortexId, _Generation, _SpecConstraint, Sensors,
                              [], ActuatorAcc, NeuronIdAcc) ->
  [write(S) || S <- Sensors],
  [write(A) || A <- ActuatorAcc],
  NeuronIdAcc.

construct_initial_neurons(CortexId, Generation, SpecConstraint,
                          [NeuronId | NeuronIds], Sensors0, Actuator) ->
  {UpdatedSensors, NeuronInputSpecs} =
    case rand:uniform() >= 0.5 of
      true ->
        S0 = neurev_rand:pick(Sensors0),
        S = S0#sensor{fanout_ids = [NeuronId | S0#sensor.fanout_ids]},
        Sensors = lists:keyreplace(S#sensor.id, 2, Sensors0, S),
        InputSpecs = [{S#sensor.id, S#sensor.vector_length}],
        {Sensors, InputSpecs};
      false ->
        Sensors = [S#sensor{fanout_ids = [NeuronId | S#sensor.fanout_ids]}
                   || S <- Sensors0],
        InputSpecs = [{S#sensor.id, S#sensor.vector_length} || S <- Sensors],
        {Sensors, InputSpecs}
    end,
  construct_neuron(CortexId, Generation, SpecConstraint, NeuronId,
                   NeuronInputSpecs, [Actuator#actuator.id]),
  construct_initial_neurons(CortexId, Generation, SpecConstraint, NeuronIds,
                            UpdatedSensors, Actuator);
construct_initial_neurons(_CortexId, _Generation, _SpecConstraint, [], Sensors,
                          _Actuator) ->
  Sensors.

construct_neuron(CortexId, Generation, SpecConstraint, NeuronId, InputSpecs,
                 OutputIds) ->
  Input = create_input(InputSpecs, []),
  ActivationFunction =
    generate_neuron_activation_function(
      SpecConstraint#constraint.neural_activation_functions),
  RecurrentOutputIds = calculate_recurrent_output_ids(NeuronId, OutputIds, []),
  Neuron = #neuron{ id = NeuronId
                  , cortex_id = CortexId
                  , generation = Generation
                  , activation_function = ActivationFunction
                  , input = Input
                  , output = OutputIds
                  , recurrent_output_ids = RecurrentOutputIds
                  },
  write(Neuron).

create_input([{InputId, InputVectorLength} | InputSpecs], Acc) ->
  Weights = create_neural_weights(InputVectorLength, []),
  create_input(InputSpecs, [{InputId, Weights} | Acc]);
create_input([], Acc) ->
  Acc.

create_neural_weights(0, Acc) ->
  Acc;
create_neural_weights(Index, Acc) ->
  W = rand:uniform() - 0.5,
  create_neural_weights(Index - 1, [W | Acc]).

generate_neuron_activation_function(ActivationFunctions) ->
  case ActivationFunctions of
    [] ->
      tanh;
    _ ->
      neurev_rand:pick(ActivationFunctions)
  end.

calculate_recurrent_output_ids(SelfId, [OutputId | Ids], Acc) ->
  case OutputId of
    {_, actuator} ->
      calculate_recurrent_output_ids(SelfId, Ids, Acc);
    OutputId ->
      {{TLI, _}, _NodeType} = SelfId,
      {{LI, _}, _} = OutputId,
      case LI =< TLI of
        true ->
          calculate_recurrent_output_ids(SelfId, Ids, [OutputId | Acc]);
        false ->
          calculate_recurrent_output_ids(SelfId, Ids, Acc)
      end
  end;
calculate_recurrent_output_ids(_SelfId, [], Acc) ->
  lists:reverse(Acc).

generate_ids(0, Acc) ->
  Acc;
generate_ids(Index, Acc) ->
  Id = neurev_rand:generate_id(),
  generate_ids(Index - 1, [Id | Acc]).

update_fingerprint(AgentId) ->
  A = read({agent, AgentId}),
  Cortex = read({cortex, A#agent.cortex_id}),
  GeneralizedSensors =
    [(read({sensor, SensorId}))#sensor{ id = undefined
                                      , cortex_id = undefined
                                      }
     || SensorId <- Cortex#cortex.sensor_ids],
  GeneralizedActuators =
    [(read({actuator, ActuatorId}))#actuator{ id = undefined
                                            , cortex_id = undefined
                                            }
     || ActuatorId <- Cortex#cortex.actuator_ids],
  GeneralizedPattern =
    [{LayerIndex, length(LNIds)}
     || {LayerIndex, LNIds} <- A#agent.pattern],
  GeneralizedEvoHist = generalize_evo_hist(A#agent.evo_hist, []),
  Fingerprint = { GeneralizedPattern
                , GeneralizedEvoHist
                , GeneralizedSensors
                , GeneralizedActuators
                },
  write(A#agent{fingerprint = Fingerprint}).

generalize_evo_hist([ { MO
                      , {{ALI, _AUId}, AType}
                      , {{BLI, _BUId}, BType}
                      , {{CLI, _CUId}, CType}
                      } | EvoHist ], Acc) ->
  generalize_evo_hist(EvoHist, [ { MO
                                 , {ALI, AType}
                                 , {BLI, BType}
                                 , {CLI, CType}
                                 } | Acc]);
generalize_evo_hist([ { MO
                      , {{ALI, _AUId}, AType}
                      , {{BLI, _BUId}, BType}
                      } | EvoHist ], Acc) ->
  generalize_evo_hist(EvoHist, [ { MO
                                , {ALI, AType}
                                , {BLI, BType}
                                } | Acc]);
generalize_evo_hist([ { MO
                      , {{ALI, _AUId}, AType}
                      } | EvoHist ], Acc) ->
  generalize_evo_hist(EvoHist, [ { MO
                                , {ALI, AType}
                                } | Acc]);
generalize_evo_hist([], Acc) ->
  lists:reverse(Acc).

read(TableKey = {_Table, _Key}) ->
  case mnesia:read(TableKey) of
    [] ->
      undefined;
    [R] ->
      R
  end.

write(R) ->
  mnesia:write(R).

delete(TableKey = {_Table, _Key}) ->
  mnesia:delete(TableKey).

log(AgentId) ->
  Agent = read({agent, AgentId}),
  Cortex = read({cortex, Agent#agent.cortex_id}),
  ?LOG_INFO("agent=~p", [Agent]),
  ?LOG_INFO("cortex=~p", [Cortex]),
  [?LOG_INFO("sensor=~p", [read({sensor, Id})])
   || Id <- Cortex#cortex.sensor_ids],
  [?LOG_INFO("actuator=~p", [read({actuator, Id})])
   || Id <- Cortex#cortex.actuator_ids],
  [?LOG_INFO("neuron=~p", [read({neuron, Id})])
   || Id <- Cortex#cortex.neuron_ids],
  ok.

delete_agent(AgentId) ->
  Agent = read({agent, AgentId}),
  Cortex = read({cortex, Agent#agent.cortex_id}),
  [delete({neuron, Id}) || Id <- Cortex#cortex.neuron_ids],
  [delete({sensor, Id}) || Id <- Cortex#cortex.sensor_ids],
  [delete({actuator, Id}) || Id <- Cortex#cortex.actuator_ids],
  delete({cortex, Agent#agent.cortex_id}),
  delete({agent, AgentId}).

delete_agent_safe(AgentId) ->
  F = fun() ->
          Agent = read({agent, AgentId}),
          Specie = read({specie, Agent#agent.specie_id}),
          AgentIds = Specie#specie.agent_ids,
          write(Specie#specie{agent_ids = lists:delete(AgentId, AgentIds)}),
          delete_agent(AgentId)
      end,
  Result = mnesia:transaction(F),
  ?LOG_DEBUG("delete_agent_safe agent_id=~p result=~p", [AgentId, Result]).

clone_agent(AgentId, CloneAgentId) ->
  F = fun() ->
          Agent = read({agent, AgentId}),
          Cortex = read({cortex, Agent#agent.cortex_id}),
          Ets = ets:new(idsNCloneIds, [set, private]),
          ets:insert(Ets, {threshold, threshold}),
          ets:insert(Ets, {AgentId, CloneAgentId}),
          [CloneCortexId] = map_ids(Ets, [Agent#agent.cortex_id], []),
          CloneNeuronIds = map_ids(Ets, Cortex#cortex.neuron_ids, []),
          CloneSensorIds = map_ids(Ets, Cortex#cortex.sensor_ids, []),
          CloneActuatorIds = map_ids(Ets, Cortex#cortex.actuator_ids, []),
          clone_neurons(Ets, Cortex#cortex.neuron_ids),
          clone_sensors(Ets, Cortex#cortex.sensor_ids),
          clone_actuators(Ets, Cortex#cortex.actuator_ids),
          write(Cortex#cortex{ id = CloneCortexId
                             , agent_id = CloneAgentId
                             , sensor_ids = CloneSensorIds
                             , actuator_ids = CloneActuatorIds
                             , neuron_ids = CloneNeuronIds
                             }),
          write(Agent#agent{ id = CloneAgentId
                           , cortex_id = CloneCortexId
                           }),
          ets:delete(Ets)
      end,
  mnesia:transaction(F).

map_ids(Ets, [Id | Ids], Acc) ->
  CloneId = case Id of
              {{LayerIndex, _NumId}, Type} ->
                {{LayerIndex, neurev_rand:generate_id()}, Type};
              {_NumId, Type} ->
                {neurev_rand:generate_id(), Type}
            end,
  ets:insert(Ets, {Id, CloneId}),
  map_ids(Ets, Ids, [CloneId | Acc]);
map_ids(_Ets, [], Acc) ->
  Acc.

clone_sensors(Ets, [Id | Ids]) ->
  S = read({sensor, Id}),
  CloneId = ets:lookup_element(Ets, Id, 2),
  CloneCortexId = ets:lookup_element(Ets, S#sensor.cortex_id, 2),
  CloneFanoutIds = [ets:lookup_element(Ets, FanoutId, 2)
                    || FanoutId <- S#sensor.fanout_ids],
  write(S#sensor{ id = CloneId
                , cortex_id = CloneCortexId
                , fanout_ids = CloneFanoutIds
                }),
  clone_sensors(Ets, Ids);
clone_sensors(_Ets, []) ->
  ok.

clone_actuators(Ets, [Id | Ids]) ->
  A = read({actuator, Id}),
  CloneId = ets:lookup_element(Ets, Id, 2),
  CloneCortexId = ets:lookup_element(Ets, A#actuator.cortex_id, 2),
  CloneFaninIds = [ets:lookup_element(Ets, FaninId, 2)
                   || FaninId <- A#actuator.fanin_ids],
  write(A#actuator{ id = CloneId
                  , cortex_id = CloneCortexId
                  , fanin_ids = CloneFaninIds
                  }),
  clone_actuators(Ets, Ids);
clone_actuators(_Ets, []) ->
  ok.

clone_neurons(Ets, [Id | Ids]) ->
  N = read({neuron, Id}),
  CloneId = ets:lookup_element(Ets, Id, 2),
  CloneCortexId = ets:lookup_element(Ets, N#neuron.cortex_id, 2),
  CloneInput = [{ets:lookup_element(Ets, InputId, 2), Weights}
                || {InputId, Weights} <- N#neuron.input],
  CloneOutput = [ets:lookup_element(Ets, OutputId, 2)
                 || OutputId <- N#neuron.output],
  CloneROIds = [ets:lookup_element(Ets, ROId, 2)
                || ROId <- N#neuron.recurrent_output_ids],
  write(N#neuron{ id = CloneId
                , cortex_id = CloneCortexId
                , input = CloneInput
                , output = CloneOutput
                , recurrent_output_ids = CloneROIds
                }),
  clone_neurons(Ets, Ids);
clone_neurons(_Ets, []) ->
  ok.
