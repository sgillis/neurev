-module(neurev_sensor).

-export([ gen/2
        , prep/1
        , rng/2
        , xor_get_input/2
        ]).

-include("neurev.hrl").
-include_lib("kernel/include/logger.hrl").


%% API ---------------------------------------------------------------

gen(ExoPid, Node) ->
  spawn(Node, ?MODULE, prep, [ExoPid]).

prep(ExoPid) ->
  ?LOG_DEBUG("sensor=~p waiting for setup", [self()]),
  receive
    {ExoPid, {Id, CortexPid, Scape, SensorName, VectorLength, FanoutPids}} ->
      ?LOG_DEBUG("sensor=~p setup received", [self()]),
      loop(Id, ExoPid, CortexPid, Scape, SensorName, VectorLength, FanoutPids)
  end.

loop(Id, ExoPid, CortexPid, Scape, SensorName, VectorLength, FanoutPids) ->
  receive
    {CortexPid, sync} ->
      SensoryVector = neurev_sensor:SensorName(VectorLength, Scape),
      [Pid ! {self(), forward, SensoryVector} || Pid <- FanoutPids],
      ?LOG_DEBUG("sensor=~p received sync, sent vector=~p",
                   [Id, SensoryVector]),
      loop(Id, ExoPid, CortexPid, Scape, SensorName, VectorLength, FanoutPids);
    {ExoPid, terminate} ->
      ok
  end.

rng(VectorLength, _Scape) ->
  rng1(VectorLength, []).

rng1(0, Acc) ->
  Acc;
rng1(VectorLength, Acc) ->
  rng1(VectorLength - 1, [rand:uniform() | Acc]).

xor_get_input(VectorLength, Scape) ->
  Scape ! {self(), sense},
  receive
    {Scape, percept, SensoryVector} ->
      case length(SensoryVector) == VectorLength of
        true ->
          SensoryVector;
        false ->
          ?LOG_WARNING("Error in neurev_sensor:xor_get_input/2 "
                       "VectorLength=~p SensoryVector=~p",
                       [VectorLength, SensoryVector]),
          lists:duplicate(VectorLength, 0)
      end
  end.
