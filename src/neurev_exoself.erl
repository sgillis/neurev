-module(neurev_exoself).

-export([ map/1
        ]).

-include("neurev.hrl").

map(Genotype) ->
  ets:new(idspids, [set, named_table]),
  Cortex = Genotype#genotype.cortex,
  SensorIds = Cortex#cortex.sensor_ids,
  ActuatorIds = Cortex#cortex.actuator_ids,
  NeuronIds = Cortex#cortex.neuron_ids,
  spawn_cerebral_units(neurev_cortex, [Cortex#cortex.id]),
  spawn_cerebral_units(neurev_sensor, SensorIds),
  spawn_cerebral_units(neurev_actuator, ActuatorIds),
  spawn_cerebral_units(neurev_neuron, NeuronIds),
  CerebralUnits = [ Genotype#genotype.sensor
                  , Genotype#genotype.actuator
                    | Genotype#genotype.neurons
                  ],
  link_cerebral_units(CerebralUnits),
  link_cortex(Cortex),
  CortexPid = ets:lookup_element(idspids, Cortex#cortex.id, 2),
  receive
    {CortexPid, backup, NeuronIdsWeights} ->
      UpdatedGenotype = update_genotype(Genotype, NeuronIdsWeights),
      ets:delete(idspids),
      UpdatedGenotype
  end.

spawn_cerebral_units(CerebralUnitType, [Id | Ids]) ->
  Pid = CerebralUnitType:gen(self(), node()),
  ets:insert(idspids, {Id, Pid}),
  ets:insert(idspids, {Pid, Id}),
  spawn_cerebral_units(CerebralUnitType, Ids);
spawn_cerebral_units(_CerebralUnitType, []) ->
  true.

link_cerebral_units([R | Records]) when is_record(R, sensor) ->
  SensorId = R#sensor.id,
  SensorPid = lookup_pid(SensorId),
  CortexPid = lookup_pid(R#sensor.cortex_id),
  SensorName = R#sensor.name,
  FanoutIds = R#sensor.fanout_ids,
  FanoutPids = [lookup_pid(Id) || Id <- FanoutIds],
  SensorPid ! {self(), {SensorId, CortexPid, SensorName, R#sensor.vector_length, FanoutPids}},
  link_cerebral_units(Records);
link_cerebral_units([R | Records]) when is_record(R, actuator) ->
  ActuatorId = R#actuator.id,
  ActuatorPid = lookup_pid(ActuatorId),
  CortexPid = lookup_pid(R#actuator.cortex_id),
  ActuatorName = R#actuator.name,
  FaninIds = R#actuator.fanin_ids,
  FaninPids = [lookup_pid(Id) || Id <- FaninIds],
  ActuatorPid ! {self(), {ActuatorId, CortexPid, ActuatorName, FaninPids}},
  link_cerebral_units(Records);
link_cerebral_units([R | Records]) when is_record(R, neuron) ->
  NeuronId = R#neuron.id,
  NeuronPid = lookup_pid(NeuronId),
  CortexPid = lookup_pid(R#neuron.cortex_id),
  ActivationF = R#neuron.activation_function,
  Inputs = R#neuron.input,
  Outputs = R#neuron.output,
  InputPids = neuron_input2neuron_pid_input(Inputs, []),
  OutputPids = [lookup_pid(Id) || Id <- Outputs],
  NeuronPid ! {self(), { NeuronId
                       , CortexPid
                       , ActivationF
                       , InputPids
                       , OutputPids
                       }
              },
  link_cerebral_units(Records);
link_cerebral_units([]) ->
  ok.

link_cortex(Cortex) ->
  CortexId = Cortex#cortex.id,
  CortexPid = lookup_pid(CortexId),
  SensorIds = Cortex#cortex.sensor_ids,
  ActuatorIds = Cortex#cortex.actuator_ids,
  NeuronIds = Cortex#cortex.neuron_ids,
  SensorPids = [lookup_pid(Id) || Id <- SensorIds],
  ActuatorPids = [lookup_pid(Id) || Id <- ActuatorIds],
  NeuronPids = [lookup_pid(Id) || Id <- NeuronIds],
  CortexPid ! { self()
              , { CortexId
                , SensorPids
                , ActuatorPids
                , NeuronPids
                }
              , 10
              }.

update_genotype(Genotype0, [{NeuronId, PidInput} | Weights]) ->
  Neurons0 = Genotype0#genotype.neurons,
  Neuron0 = lists:keyfind(NeuronId, 2, Neurons0),
  Inputs = neuron_pid_input2neuron_input(PidInput, []),
  Neuron = Neuron0#neuron{input = Inputs},
  Neurons = lists:keyreplace(NeuronId, 2, Neurons0, Neuron),
  Genotype = Genotype0#genotype{neurons = Neurons},
  update_genotype(Genotype, Weights);
update_genotype(Genotype, []) ->
  Genotype.

lookup_pid(Id) ->
  ets:lookup_element(idspids, Id, 2).

neuron_input2neuron_pid_input([{bias, Bias}], Acc) ->
  lists:reverse([Bias | Acc]);
neuron_input2neuron_pid_input([{Id, Weights} | Inputs], Acc) ->
  PidInput = {ets:lookup_element(idspids, Id, 2), Weights},
  neuron_input2neuron_pid_input(Inputs, [PidInput | Acc]).

neuron_pid_input2neuron_input([{Pid, Weights} | InputPids], Acc) ->
  Input = {ets:lookup_element(idspids, Pid, 2), Weights},
    neuron_pid_input2neuron_input(InputPids, [Input | Acc]);
neuron_pid_input2neuron_input([Bias], Acc) ->
  lists:reverse([{bias, Bias} | Acc]).
