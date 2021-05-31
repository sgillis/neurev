-module(neurev_genotype_mutator).

-export([ link/3
        , cutlink/3
        , mutate_weights/1
        , add_bias/1
        , remove_bias/1
        , mutate_af/1
        , add_outlink/1
        , add_inlink/1
        , add_sensor_link/1
        ]).

-include("neurev.hrl").

-define(DELTA_MULTIPLIER,math:pi()*2).
-define(SAT_LIMIT,math:pi()*2).

link(AgentId, From, To) ->
  case {From, To} of
    {{_FromId, neuron}, {_ToId, neuron}} ->
      link_neuron_neuron(AgentId, From, To);
    {{_FromId, sensor}, {_ToId, neuron}} ->
      link_sensor_neuron(AgentId, From, To);
    {{_FromId, neuron}, {_ToId, actuator}} ->
      link_neuron_actuator(AgentId, From, To)
  end.

link_neuron_neuron(AgentId, From, To) ->
  A = neurev_genotype:read({agent, AgentId}),
  Generation = A#agent.generation,
  FromNeuron = neurev_genotype:read({neuron, From}),
  UFromNeuron = link_from_neuron(FromNeuron, To, Generation),
  neurev_genotype:write(UFromNeuron),
  ToNeuron = neurev_genotype:read({neuron, To}),
  FromOVL = 1,
  UToNeuron = link_to_neuron(From, FromOVL, ToNeuron, Generation),
  neurev_genotype:write(UToNeuron).

link_from_neuron(FromNeuron, To, Generation) ->
  {{FromLI, _}, _} = FromNeuron#neuron.id,
  {{ToLI, _}, _} = To,
  FromOutputIds = FromNeuron#neuron.output,
  FromROIds = FromNeuron#neuron.recurrent_output_ids,
  case lists:member(To, FromOutputIds) of
    true ->
      exit("Error link_from_neuron ~p already member of ~p",
           [To, FromNeuron#neuron.id]);
    false ->
      {UFromOutputIds, UFromROIds} =
        case FromLI >= ToLI of
          true ->
            {[To | FromOutputIds], [To | FromROIds]};
          false ->
            {[To | FromOutputIds], FromROIds}
        end,
      FromNeuron#neuron{ output = UFromOutputIds
                       , recurrent_output_ids = UFromROIds
                       , generation = Generation
                       }
  end.

link_to_neuron(From, FromOVL, ToNeuron, Generation) ->
  ToInput = ToNeuron#neuron.input,
  case lists:keymember(From, 1, ToInput) of
    true ->
      exit("Error link_to_neuron ~p already member of ~p",
           [From, ToNeuron#neuron.id]);
    false ->
      UToInput = [{From, neurev_genotype:create_neural_weights(FromOVL, [])}
                  | ToInput],
      ToNeuron#neuron{ input = UToInput
                     , generation = Generation
                     }
  end.

link_sensor_neuron(AgentId, SensorId, NeuronId) ->
  A = neurev_genotype:read({agent, AgentId}),
  Generation = A#agent.generation,
  Sensor = neurev_genotype:read({sensor, SensorId}),
  USensor = link_from_sensor(Sensor, NeuronId),
  neurev_genotype:write(USensor),
  Neuron = neurev_genotype:read({neuron, NeuronId}),
  FromOVL = Sensor#sensor.vector_length,
  UNeuron = link_to_neuron(SensorId, FromOVL, Neuron, Generation),
  neurev_genotype:write(UNeuron).

link_from_sensor(Sensor, ToId) ->
  FromFanoutIds = Sensor#sensor.fanout_ids,
  case lists:member(ToId, FromFanoutIds) of
    true ->
      exit("Error in link_from_sensor ~p ~p", [Sensor#sensor.id, ToId]);
    false ->
      Sensor#sensor{ fanout_ids = [ToId | FromFanoutIds]}
  end.

link_neuron_actuator(AgentId, NeuronId, ActuatorId) ->
  A = neurev_genotype:read({agent, AgentId}),
  Generation = A#agent.generation,
  Neuron = neurev_genotype:read({neuron, NeuronId}),
  UNeuron = link_from_neuron(Neuron, ActuatorId, Generation),
  neurev_genotype:write(UNeuron),
  Actuator = neurev_genotype:read({actuator, ActuatorId}),
  FaninIds = Actuator#actuator.fanin_ids,
  case length(FaninIds) >= Actuator#actuator.vector_length of
    true ->
      exit("Actuator already fully connected ~p", [ActuatorId]);
    false ->
      UFaninIds = [NeuronId | FaninIds],
      neurev_genotype:write(Actuator#actuator{fanin_ids = UFaninIds})
  end.

cutlink(AgentId, From, To) ->
  case {From, To} of
    {{_FromId, neuron}, {_ToId, neuron}} ->
      cutlink_neuron_neuron(AgentId, From, To);
    {{_FromId, sensor}, {_ToId, neuron}} ->
      cutlink_sensor_neuron(AgentId, From, To);
    {{_FromId, neuron}, {_ToId, actuator}} ->
      cutlink_neuron_actuator(AgentId, From, To)
  end.

cutlink_neuron_neuron(AgentId, From, To) ->
  A = neurev_genotype:read({agent, AgentId}),
  Generation = A#agent.generation,
  FromNeuron = neurev_genotype:read({neuron, From}),
  UFromNeuron = cutlink_from_neuron(FromNeuron, To, Generation),
  neurev_genotype:write(UFromNeuron),
  ToNeuron = neurev_genotype:read({neuron, To}),
  UToNeuron = cutlink_to_neuron(From, ToNeuron, Generation),
  neurev_genotype:write(UToNeuron).

cutlink_from_neuron(FromNeuron, ToId, Generation) ->
  FromOutputIds = FromNeuron#neuron.output,
  FromROIds = FromNeuron#neuron.recurrent_output_ids,
  case lists:member(ToId, FromOutputIds) of
    true ->
      UFromOutputIds = FromOutputIds -- [ToId],
      UFromROIds = FromROIds -- [ToId],
      FromNeuron#neuron{ output = UFromOutputIds
                       , recurrent_output_ids = UFromROIds
                       , generation = Generation
                       };
    false ->
      exit("Error in cutlink_from_neuron ~p ~p", [FromNeuron#neuron.id, ToId])
  end.

cutlink_to_neuron(FromId, ToNeuron, Generation) ->
  ToInput = ToNeuron#neuron.input,
  case lists:keymember(FromId, 1, ToInput) of
    true ->
      UToInput = lists:keydelete(FromId, 1, ToInput),
      ToNeuron#neuron{ input = UToInput
                     , generation = Generation
                     };
    false ->
      exit("Error in cutlink_to_neuron ~p ~p", [FromId, ToNeuron#neuron.id])
  end.

cutlink_sensor_neuron(AgentId, SensorId, NeuronId) ->
  A = neurev_genotype:read({agent, AgentId}),
  Generation = A#agent.generation,
  Sensor = neurev_genotype:read({sensor, SensorId}),
  USensor = cutlink_from_sensor(Sensor, NeuronId, Generation),
  neurev_genotype:write(USensor),
  Neuron = neurev_genotype:read({neuron, NeuronId}),
  UNeuron = cutlink_to_neuron(SensorId, Neuron, Generation),
  neurev_genotype:write(UNeuron).

cutlink_from_sensor(Sensor, ToId, _Generation) ->
  FromFanoutIds = Sensor#sensor.fanout_ids,
  case lists:member(ToId, FromFanoutIds) of
    true ->
      UFromFanoutIds = FromFanoutIds -- [ToId],
      Sensor#sensor{fanout_ids = UFromFanoutIds};
    false ->
      exit("Error in cutlink_from_sensor ~p ~p", [Sensor#sensor.id, ToId])
  end.

cutlink_neuron_actuator(AgentId, NeuronId, ActuatorId) ->
  A = neurev_genotype:read({agent, AgentId}),
  Generation = A#agent.generation,
  Neuron = neurev_genotype:read({neuron, NeuronId}),
  UNeuron = cutlink_from_neuron(Neuron, ActuatorId, Generation),
  neurev_genotype:write(UNeuron),
  Actuator = neurev_genotype:read({actuator, ActuatorId}),
  UActuator = cutlink_to_actuator(NeuronId, Actuator, Generation),
  neurev_genotype:write(UActuator).

cutlink_to_actuator(FromId, Actuator, _Generation) ->
  FaninIds = Actuator#actuator.fanin_ids,
  case lists:member(FromId, FaninIds) of
    true ->
      UFaninIds = FaninIds -- [FromId],
      Actuator#actuator{fanin_ids = UFaninIds};
    false ->
      exit("Error in cutlink_to_actuator ~p ~p",
           [FromId, Actuator#actuator.id])
  end.

mutate_weights(AgentId) ->
  A = neurev_genotype:read({agent, AgentId}),
  CortexId = A#agent.cortex_id,
  Cortex = neurev_genotype:read({cortex, CortexId}),
  NeuronIds = Cortex#cortex.neuron_ids,
  NeuronId = neurev_rand:pick(NeuronIds),
  Neuron = neurev_genotype:read({neuron, NeuronId}),
  Input = Neuron#neuron.input,
  UInput = perturb_input(Input),
  UNeuron = Neuron#neuron{input = UInput},
  EvoHist = A#agent.evo_hist,
  UEvoHist = [{mutate_weights, NeuronId} | EvoHist],
  UA = A#agent{evo_hist = UEvoHist},
  neurev_genotype:write(UNeuron),
  neurev_genotype:write(UA).

perturb_input(Input) ->
  TotWeights = lists:sum([length(Weights) || {_InputId, Weights} <- Input]),
  MP = 1 / math:sqrt(TotWeights),
  perturb_input(MP, Input, []).

perturb_input(MP, [{InputId, Weights} | Input], Acc) ->
  UWeights = perturb_weights(MP, Weights, []),
  perturb_input(MP, Input, [{InputId, UWeights} | Acc]);
perturb_input(_MP, [], Acc) ->
  lists:reverse(Acc).

perturb_weights(MP, [W | Weights], Acc) ->
  UWeights = case rand:uniform() < MP of
               true ->
                 sat((rand:uniform() - 0.5) * ?DELTA_MULTIPLIER + W,
                     -?SAT_LIMIT, ?SAT_LIMIT);
               false ->
                 W
             end,
  perturb_weights(MP, Weights, [UWeights | Acc]);
perturb_weights(_MP, [], Acc) ->
  lists:reverse(Acc).

sat(Val, Min, Max) ->
  if
    Val < Min -> Min;
    Val > Max -> Max;
    true -> Val
  end.

add_bias(AgentId) ->
  A = neurev_genotype:read({agent, AgentId}),
  CortexId = A#agent.cortex_id,
  Cortex = neurev_genotype:read({cortex, CortexId}),
  NeuronIds = Cortex#cortex.neuron_ids,
  NeuronId = neurev_rand:pick(NeuronIds),
  Generation = A#agent.generation,
  Neuron = neurev_genotype:read({neuron, NeuronId}),
  Input = Neuron#neuron.input,
  case lists:keymember(bias, 1, Input) of
    true ->
      exit("Error in add_bias ~p", [NeuronId]);
    false ->
      UInput = lists:append(Input, [{bias, [rand:uniform() - 0.5]}]),
      UNeuron = Neuron#neuron{ input = UInput
                             , generation = Generation
                             },
      EvoHist = A#agent.evo_hist,
      UEvoHist = [{add_bias, NeuronId} | EvoHist],
      UA = A#agent{ evo_hist = UEvoHist },
      neurev_genotype:write(UNeuron),
      neurev_genotype:write(UA)
  end.

remove_bias(AgentId) ->
  A = neurev_genotype:read({agent, AgentId}),
  CortexId = A#agent.cortex_id,
  Cortex = neurev_genotype:read({cortex, CortexId}),
  NeuronIds = Cortex#cortex.neuron_ids,
  NeuronId = neurev_rand:pick(NeuronIds),
  Generation = A#agent.generation,
  Neuron = neurev_genotype:read({neuron, NeuronId}),
  Input = Neuron#neuron.input,
  case lists:keymember(bias, 1, Input) of
    false ->
      exit("Erro in remove_bias ~p", [NeuronId]);
    true ->
      UInput = lists:keydelete(bias, 1, Input),
      UNeuron = Neuron#neuron{ input = UInput
                             , generation = Generation
                             },
      EvoHist = A#agent.evo_hist,
      UEvoHist = [{remove_bias, NeuronId}, EvoHist],
      UA = A#agent{evo_hist = UEvoHist},
      neurev_genotype:write(UNeuron),
      neurev_genotype:write(UA)
  end.

mutate_af(AgentId) ->
  A = neurev_genotype:read({agent, AgentId}),
  CortexId = A#agent.cortex_id,
  Cortex = neurev_genotype:read({cortex, CortexId}),
  NeuronIds = Cortex#cortex.neuron_ids,
  NeuronId = neurev_rand:pick(NeuronIds),
  Generation = A#agent.generation,
  Neuron = neurev_genotype:read({neuron, NeuronId}),
  AF = Neuron#neuron.activation_function,
  AFs = (A#agent.constraint)#constraint.neural_activation_functions -- [AF],
  NewAF = neurev_genotype:generate_neuron_af(AFs),
  UNeuron = Neuron#neuron{ activation_function = NewAF
                         , generation = Generation
                         },
  EvoHist = A#agent.evo_hist,
  UEvoHist = [{mutate_af, AgentId} | EvoHist],
  UAgent = A#agent{evo_hist = UEvoHist},
  neurev_genotype:write(UNeuron),
  neurev_genotype:write(UAgent).

add_outlink(AgentId) ->
  A = neurev_genotype:read({agent, AgentId}),
  CortexId = A#agent.cortex_id,
  Cortex = neurev_genotype:read({cortex, CortexId}),
  NeuronIds = Cortex#cortex.neuron_ids,
  ActuatorIds = Cortex#cortex.actuator_ids,
  NeuronId = neurev_rand:pick(NeuronIds),
  Neuron = neurev_genotype:read({neuron, NeuronId}),
  OutputIds = Neuron#neuron.output,
  case lists:append(ActuatorIds, NeuronIds) -- OutputIds of
    [] ->
      exit("add_outlink neuron already connected to all ids");
    AvailableIds ->
      ToId = neurev_rand:pick(AvailableIds),
      link(AgentId, NeuronId, ToId),
      EvoHist = A#agent.evo_hist,
      UEvoHist = [{add_outlink, NeuronId, ToId} | EvoHist],
      UA = A#agent{evo_hist = UEvoHist},
      neurev_genotype:write(UA)
  end.

add_inlink(AgentId) ->
  A = neurev_genotype:read({agent, AgentId}),
  CortexId = A#agent.cortex_id,
  Cortex = neurev_genotype:read({cortex, CortexId}),
  NeuronIds = Cortex#cortex.neuron_ids,
  SensorIds = Cortex#cortex.sensor_ids,
  NeuronId = neurev_rand:pick(NeuronIds),
  Neuron = neurev_genotype:read({neuron, NeuronId}),
  {I_Ids, _WeightList} = lists:unzip(Neuron#neuron.input),
  case lists:append(SensorIds, NeuronIds) -- I_Ids of
    [] ->
      exit("add_inlink: neuron already connected from all ids");
    AvailableIds ->
      FromId = neurev_rand:pick(AvailableIds),
      link(AgentId, FromId, NeuronId),
      EvoHist = A#agent.evo_hist,
      UEvoHist = [{add_inlink, FromId, NeuronId} | EvoHist],
      neurev_genotype:write(A#agent{evo_hist = UEvoHist})
  end.

add_sensor_link(AgentId) ->
  A = neurev_genotype:read({agent, AgentId}),
  CortexId = A#agent.cortex_id,
  Cortex = neurev_genotype:read({cortex, CortexId}),
  NeuronIds = Cortex#cortex.neuron_ids,
  SensorIds = Cortex#cortex.sensor_ids,
  SensorId = neurev_rand:pick(SensorIds),
  S = neurev_genotype:read({sensor, SensorId}),
  case NeuronIds -- S#sensor.fanout_ids of
    [] ->
      exit("add_sensor_link: sensor already connected to all NeuronIds");
    AvailableIds ->
      NeuronId = neurev_rand:pick(AvailableIds),
      link(AgentId, SensorId, NeuronId),
      EvoHist = A#agent.evo_hist,
      UEvoHist = [{add_sensor_link, SensorId, NeuronId} | EvoHist],
      neurev_genotype:write(A#agent{evo_hist = UEvoHist})
  end.
