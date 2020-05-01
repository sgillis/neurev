-module(neurev_cortex).

-export([ gen/2
        , prep/1
        , loop/9
        ]).

-include("neurev.hrl").
-include_lib("kernel/include/logger.hrl").

gen(ExoSelfPid, Node) ->
  spawn(Node, ?MODULE, prep, [ExoSelfPid]).

prep(ExoPid) ->
  rand:seed(exs1024, erlang:timestamp()),
  ?LOG_DEBUG("cortex=~p waiting for setup", [self()]),
  receive
    {ExoPid, Id, SensorPids, NeuronPids, ActuatorPids} ->
      ?LOG_DEBUG("cortex=~p setup received", [self()]),
      put(start_time, erlang:timestamp()),
      [SensorPid ! {self(), sync} || SensorPid <- SensorPids],
      loop(Id, ExoPid, SensorPids, {ActuatorPids, ActuatorPids}, NeuronPids,
           1, 0, 0, active)
  end.

loop(Id, ExoPid, SensorPids, {[ActuatorPid | ActuatorPids], MActuatorPids},
     NeuronPids, CycleAcc, FitnessAcc, HFAcc, active) ->
  receive
    {ActuatorPid, sync, Fitness, HaltFlag} ->
      ?LOG_DEBUG("cortex received actuator=~p sync fitness=~p haltflag=~p",
                   [ActuatorPid, Fitness, HaltFlag]),
      loop(Id, ExoPid, SensorPids, {ActuatorPids, MActuatorPids},
           NeuronPids, CycleAcc, FitnessAcc + Fitness, HFAcc + HaltFlag, active);
    terminate ->
      ?LOG_DEBUG("Cortex: ~p is terminating", [Id]),
      [Pid ! {self(), terminate} || Pid <- SensorPids],
      [Pid ! {self(), terminate} || Pid <- MActuatorPids],
      [Pid ! {self(), terminate} || Pid <- NeuronPids]
  end;
loop(Id, ExoPid, SensorPids, {[], MActuatorPids}, NeuronPids, CycleAcc,
     FitnessAcc, HFAcc, active) ->
  case HFAcc > 0 of
    true ->
      TimeDiff = timer:now_diff(erlang:timestamp(), get(start_time)),
      ExoPid ! {self(), evaluation_completed, FitnessAcc, CycleAcc, TimeDiff},
      loop(Id, ExoPid, SensorPids, {MActuatorPids, MActuatorPids}, NeuronPids,
           CycleAcc, FitnessAcc, HFAcc, inactive);
    false ->
      [Pid ! {self(), sync} || Pid <- SensorPids],
      loop(Id, ExoPid, SensorPids, {MActuatorPids, MActuatorPids}, NeuronPids,
           CycleAcc + 1, FitnessAcc, HFAcc, active)
  end;
loop(Id, ExoPid, SensorPids, {MActuatorPids, MActuatorPids}, NeuronPids,
     _CycleAcc, _FitnessAcc, _HFAcc, inactive) ->
  receive
    {ExoPid, reactivate} ->
      ?LOG_DEBUG("reactivating cortex=~p", [self()]),
      put(start_time, erlang:timestamp()),
      [SensorPid ! {self(), sync} || SensorPid <- SensorPids],
      loop(Id, ExoPid, SensorPids, {MActuatorPids, MActuatorPids}, NeuronPids,
           1, 0, 0, active);
    {ExoPid, terminate} ->
      ok
  end.
