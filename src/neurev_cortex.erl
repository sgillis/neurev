-module(neurev_cortex).

-export([ gen/2
        , loop/1
        ]).

-include("neurev.hrl").

gen(ExoSelfPid, Node) ->
  spawn(Node, ?MODULE, loop, [ExoSelfPid]).

loop(ExoSelfPid) ->
  receive
    {ExoSelfPid, {Id, SensorPids, ActuatorPids, NeuronPids}, TotSteps} ->
      [SensorPid ! {self(), sync} || SensorPid <- SensorPids],
      loop(Id,
           ExoSelfPid,
           SensorPids,
           {ActuatorPids, ActuatorPids},
           NeuronPids,
           TotSteps)
  end.

loop(_Id, ExoSelfPid, SensorPids, {_ActuatorPids, MemoryActuatorPids},
     NeuronPids, 0) ->
  NeuronIdsWeights = get_backup(NeuronPids, []),
  ExoSelfPid ! {self(), backup, NeuronIdsWeights},
  [Pid ! {self(), terminate} || Pid <- SensorPids],
  [Pid ! {self(), terminate} || Pid <- MemoryActuatorPids],
  [Pid ! {self(), terminate} || Pid <- NeuronPids];
loop(Id, ExoSelfPid, SensorPids,
     {[ActuatorPid | ActuatorPids], MemoryActuatorPids},
     NeuronPids, Step) ->
  receive
    {ActuatorPid, sync} ->
      loop(Id, ExoSelfPid, SensorPids, {ActuatorPids, MemoryActuatorPids}, NeuronPids, Step);
    terminate ->
      [Pid ! {self(), terminate} || Pid <- SensorPids],
      [Pid ! {self(), terminate} || Pid <- MemoryActuatorPids],
      [Pid ! {self(), terminate} || Pid <- NeuronPids]
  end;
loop(Id, ExoSelfPid, SensorPids, {[], MemoryActuatorPids}, NeuronPids, Step) ->
  [SensorPid ! {self(), sync} || SensorPid <- SensorPids],
  loop(Id,
       ExoSelfPid,
       SensorPids,
       {MemoryActuatorPids, MemoryActuatorPids},
       NeuronPids,
       Step - 1).

get_backup([NeuronPid | NeuronPids], Acc) ->
  NeuronPid ! {self(), get_backup},
  receive
    {NeuronPid, NeuronId, WeightTuples} ->
      get_backup(NeuronPids, [{NeuronId, WeightTuples} | Acc])
  end;
get_backup([], Acc) ->
  Acc.
