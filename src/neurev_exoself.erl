-module(neurev_exoself).

-export([ prep/2
        ]).

-include("neurev.hrl").
-include_lib("kernel/include/logger.hrl").

-define(MAX_ATTEMPTS, 10).

prep(GenotypeName, Genotype) ->
  rand:seed(exs1024, erlang:timestamp()),
  IdsPids = ets:new(idspids, [set, private]),
  Cortex = Genotype#genotype.cortex,
  SensorIds = Cortex#cortex.sensor_ids,
  ActuatorIds = Cortex#cortex.actuator_ids,
  NeuronIds = Cortex#cortex.neuron_ids,
  ScapePids = spawn_scapes(IdsPids, Genotype, SensorIds, ActuatorIds),
  spawn_cerebral_units(IdsPids, neurev_cortex, [Cortex#cortex.id]),
  spawn_cerebral_units(IdsPids, neurev_sensor, SensorIds),
  spawn_cerebral_units(IdsPids, neurev_actuator, ActuatorIds),
  spawn_cerebral_units(IdsPids, neurev_neuron, NeuronIds),
  link_sensors(Genotype, SensorIds, IdsPids),
  link_actuators(Genotype, ActuatorIds, IdsPids),
  link_neurons(Genotype, NeuronIds, IdsPids),
  {SensorPids, NeuronPids, ActuatorPids} = link_cortex(IdsPids, Cortex),
  CortexPid = ets:lookup_element(IdsPids, Cortex#cortex.id, 2),
  ?LOG_DEBUG("neurev_exoself=~p prep completed", [self()]),
  loop(GenotypeName, Genotype, IdsPids, CortexPid, SensorPids, NeuronPids,
       ActuatorPids, ScapePids, 0, 0, 0, 0, 1).

loop(GenotypeName, Genotype, IdsPids, CortexPid, SensorPids, NeuronPids,
     ActuatorPids, ScapePids, HighestFitness, EvalAcc, CycleAcc, TimeAcc,
     Attempt) ->
  receive
    {CortexPid, evaluation_completed, Fitness, Cycles, Time} ->
      {UHighestFitness, UAttempt} =
        case Fitness > HighestFitness of
          true ->
            [NeuronPid ! {self(), weight_backup} || NeuronPid <- NeuronPids],
            {Fitness, 0};
          false ->
            PerturbedNeuronPids = get(perturbed),
            [NeuronPid ! {self(), weight_restore}
             || NeuronPid <- PerturbedNeuronPids],
            {HighestFitness, Attempt + 1}
        end,
      ?LOG_DEBUG("evaluation completed UHighestFitness=~p UAttempt=~p",
                   [UHighestFitness, UAttempt]),
      case UAttempt >= ?MAX_ATTEMPTS of
        true ->
          UCycleAcc = CycleAcc + Cycles,
          UTimeAcc = TimeAcc + Time,
          backup_genotype(GenotypeName, IdsPids, Genotype, NeuronPids),
          terminate_phenotype(CortexPid, SensorPids, NeuronPids, ActuatorPids,
                              ScapePids),
          ?LOG_DEBUG(
            "Cortex=~p finished training. Genotype has been backed up. "
            "Fitness=~p TotEvaluation=~p TotCycles=~p TimeAcc=~p",
            [CortexPid, UHighestFitness, EvalAcc, UCycleAcc, UTimeAcc]
           ),
          case whereis(trainer) of
            undefined ->
              ok;
            Pid ->
              Pid ! {self(), UHighestFitness, EvalAcc, UCycleAcc, UTimeAcc}
          end;
        false ->
          ?LOG_DEBUG("perturbing neurons"),
          TotNeurons = length(NeuronPids),
          MP = 1 / math:sqrt(TotNeurons),
          PerturbNeuronPids = [ NeuronPid
                                || NeuronPid <- NeuronPids,
                                   rand:uniform() < MP],
          put(perturbed, PerturbNeuronPids),
          [NeuronPid ! {self(), weight_perturb}
           || NeuronPid <- PerturbNeuronPids],

          ?LOG_DEBUG("perturbation done, reactivating"),
          CortexPid ! {self(), reactivate},
          loop(GenotypeName, Genotype, IdsPids, CortexPid, SensorPids, NeuronPids,
              ActuatorPids, ScapePids, UHighestFitness, EvalAcc + 1,
              CycleAcc + Cycles, TimeAcc + Time, UAttempt)
      end
  end.

spawn_scapes(IdsPids, Genotype, SensorIds, ActuatorIds) ->
  SensorScapes = [(neurev_genotype:get_sensor_id(Genotype, Id))#sensor.scape
                  || Id <- SensorIds],
  ActuatorScapes = [(neurev_genotype:get_actuator_id(Genotype, Id))#actuator.scape
                    || Id <- ActuatorIds],
  UniqueScapes = SensorScapes ++ (ActuatorScapes -- SensorScapes),
  SNTuples = [{neurev_scape:gen(self(), node()), ScapeName}
              || {private, ScapeName} <- UniqueScapes],
  [ets:insert(IdsPids, {ScapeName, Pid}) || {Pid, ScapeName} <- SNTuples],
  [ets:insert(IdsPids, {Pid, ScapeName}) || {Pid, ScapeName} <- SNTuples],
  [Pid ! {self(), ScapeName} || {Pid, ScapeName} <- SNTuples],
  [Pid || {Pid, _ScapeName} <- SNTuples].

link_sensors(Genotype, [SensorId | SensorIds], IdsPids) ->
  Sensor = neurev_genotype:get_sensor_id(Genotype, SensorId),
  SensorPid = ets:lookup_element(IdsPids, SensorId, 2),
  CortexPid = ets:lookup_element(IdsPids, Sensor#sensor.cortex_id, 2),
  SensorName = Sensor#sensor.name,
  FanoutIds = Sensor#sensor.fanout_ids,
  FanoutPids = [ets:lookup_element(IdsPids, Id, 2) || Id <- FanoutIds],
  Scape = case Sensor#sensor.scape of
            {private, ScapeName} ->
              ets:lookup_element(IdsPids, ScapeName, 2);
            none ->
              undefined
          end,
  SensorPid ! {self(), { SensorId
                       , CortexPid
                       , Scape
                       , SensorName
                       , Sensor#sensor.vector_length
                       , FanoutPids
                       }},
  link_sensors(Genotype, SensorIds, IdsPids);
link_sensors(_Genotype, [], _IdsPids) ->
  ok.

link_actuators(Genotype, [ActuatorId | ActuatorIds], IdsPids) ->
  Actuator = neurev_genotype:get_actuator_id(Genotype, ActuatorId),
  ActuatorPid = ets:lookup_element(IdsPids, ActuatorId, 2),
  CortexPid = ets:lookup_element(IdsPids, Actuator#actuator.cortex_id, 2),
  ActuatorName = Actuator#actuator.name,
  FaninIds = Actuator#actuator.fanin_ids,
  FaninPids = [ets:lookup_element(IdsPids, Id, 2) || Id <- FaninIds],
  Scape = case Actuator#actuator.scape of
            {private, ScapeName} ->
              ets:lookup_element(IdsPids, ScapeName, 2);
            none ->
              undefined
          end,
  ActuatorPid ! {self(), {ActuatorId, CortexPid, Scape, ActuatorName, FaninPids}},
  link_actuators(Genotype, ActuatorIds, IdsPids);
link_actuators(_Genotype, [], _IdsPids) ->
  ok.

link_neurons(Genotype, [NeuronId | NeuronIds], IdsPids) ->
  Neuron = neurev_genotype:get_neuron_id(Genotype, NeuronId),
  NeuronPid = ets:lookup_element(IdsPids, NeuronId, 2),
  CortexPid = ets:lookup_element(IdsPids, Neuron#neuron.cortex_id, 2),
  ActivationFuncName = Neuron#neuron.activation_function,
  Inputs = Neuron#neuron.input,
  Outputs = Neuron#neuron.output,
  InputPids = neuron_input2neuron_pid_input(IdsPids, Inputs, []),
  OutputPids = [ets:lookup_element(IdsPids, Id, 2) || Id <- Outputs],
  NeuronPid ! {self(), { NeuronId
                       , CortexPid
                       , ActivationFuncName
                       , InputPids
                       , OutputPids
                       }},
  link_neurons(Genotype, NeuronIds, IdsPids);
link_neurons(_Genotype, [], _IdsPids) ->
  ok.

terminate_phenotype(CortexPid, SensorPids, NeuronPids, ActuatorPids, ScapePids) ->
  [Pid ! {self(), terminate} || Pid <- SensorPids],
  [Pid ! {self(), terminate} || Pid <- ActuatorPids],
  [Pid ! {self(), terminate} || Pid <- NeuronPids],
  [Pid ! {self(), terminate} || Pid <- ScapePids],
  CortexPid ! {self(), terminate}.

backup_genotype(GenotypeName, IdsPids, Genotype, NeuronPids) ->
  NeuronInputs = get_backup(NeuronPids, []),
  UpdatedGenotype = update_genotype(IdsPids, Genotype, NeuronInputs),
  neurev_genotype:write(GenotypeName, UpdatedGenotype).

get_backup([NeuronPid | NeuronPids], Acc) ->
  NeuronPid ! {self(), get_backup},
  receive
    {NeuronPid, NeuronId, WeightTuples} ->
      get_backup(NeuronPids, [{NeuronId, WeightTuples} | Acc])
  end;
get_backup([], Acc) ->
  Acc.

spawn_cerebral_units(IdsPids, CerebralUnitType, [Id | Ids]) ->
  Pid = CerebralUnitType:gen(self(), node()),
  ets:insert(IdsPids, {Id, Pid}),
  ets:insert(IdsPids, {Pid, Id}),
  spawn_cerebral_units(IdsPids, CerebralUnitType, Ids);
spawn_cerebral_units(_IdsPids, _CerebralUnitType, []) ->
  true.

link_cortex(IdsPids, Cortex) ->
  CortexId = Cortex#cortex.id,
  CortexPid = lookup_pid(IdsPids, CortexId),
  SensorIds = Cortex#cortex.sensor_ids,
  ActuatorIds = Cortex#cortex.actuator_ids,
  NeuronIds = Cortex#cortex.neuron_ids,
  SensorPids = [lookup_pid(IdsPids, Id) || Id <- SensorIds],
  ActuatorPids = [lookup_pid(IdsPids, Id) || Id <- ActuatorIds],
  NeuronPids = [lookup_pid(IdsPids, Id) || Id <- NeuronIds],
  CortexPid ! { self()
              , CortexId
              , SensorPids
              , NeuronPids
              , ActuatorPids
              },
  {SensorPids, NeuronPids, ActuatorPids}.

update_genotype(IdsPids, Genotype0, [{NeuronId, PidInput} | Weights]) ->
  Neurons0 = Genotype0#genotype.neurons,
  Neuron0 = lists:keyfind(NeuronId, 2, Neurons0),
  Inputs = neuron_pid_input2neuron_input(IdsPids, PidInput, []),
  Neuron = Neuron0#neuron{input = Inputs},
  Neurons = lists:keyreplace(NeuronId, 2, Neurons0, Neuron),
  Genotype = Genotype0#genotype{neurons = Neurons},
  update_genotype(IdsPids, Genotype, Weights);
update_genotype(_IdsPids, Genotype, []) ->
  Genotype.

lookup_pid(IdsPids, Id) ->
  ets:lookup_element(IdsPids, Id, 2).

neuron_input2neuron_pid_input(_IdsPids, [{bias, Bias}], Acc) ->
  lists:reverse([Bias | Acc]);
neuron_input2neuron_pid_input(IdsPids, [{Id, Weights} | Inputs], Acc) ->
  PidInput = {ets:lookup_element(IdsPids, Id, 2), Weights},
  neuron_input2neuron_pid_input(IdsPids, Inputs, [PidInput | Acc]).

neuron_pid_input2neuron_input(IdsPids, [{Pid, Weights} | InputPids], Acc) ->
  Input = {ets:lookup_element(IdsPids, Pid, 2), Weights},
    neuron_pid_input2neuron_input(IdsPids, InputPids, [Input | Acc]);
neuron_pid_input2neuron_input(_IdsPids, [Bias], Acc) ->
  lists:reverse([{bias, Bias} | Acc]).
