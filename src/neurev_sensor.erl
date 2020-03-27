-module(neurev_sensor).

-export([ gen/2
        , loop/1
        , rng/1
        ]).

-include("neurev.hrl").


%% API ---------------------------------------------------------------

gen(ExoSelfPid, Node) ->
  spawn(Node, ?MODULE, loop, [ExoSelfPid]).

loop(ExoSelfPid) ->
  receive
    {ExoSelfPid, {Id, CortexPid, SensorName, VectorLength, FanoutPids}} ->
      loop(Id, CortexPid, SensorName, VectorLength, FanoutPids)
  end.

loop(Id, CortexPid, SensorName, VectorLength, FanoutPids) ->
  receive
    {CortexPid, sync} ->
      SensoryVector = neurev_sensor:SensorName(VectorLength),
      [Pid ! {self(), forward, SensoryVector} || Pid <- FanoutPids],
      loop(Id, CortexPid, SensorName, VectorLength, FanoutPids);
    {CortexPid, terminate} ->
      ok
  end.

rng(VectorLength) ->
  rng(VectorLength, []).

rng(0, Acc) ->
  Acc;
rng(VectorLength, Acc) ->
  rng(VectorLength - 1, [rand:uniform() | Acc]).
