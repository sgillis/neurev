-module(neurev_actuator).

-export([ gen/2
        , loop/1
        , pts/1
        ]).

-include("neurev.hrl").


%% API ---------------------------------------------------------------

gen(ExoSelfPid, Node) ->
  spawn(Node, ?MODULE, loop, [ExoSelfPid]).

loop(ExoSelfPid) ->
  receive
    {ExoSelfPid, {Id, CortexPid, ActuatorName, FaninPids}} ->
      loop(Id, CortexPid, ActuatorName, {FaninPids, FaninPids}, [])
  end.

loop(Id, CortexPid, ActuatorName, {[FromPid | FaninPids], MemoryFaninPids},
     Acc) ->
  receive
    {FromPid, forward, Input} ->
      loop(Id, CortexPid, ActuatorName, {FaninPids, MemoryFaninPids},
           lists:append(Input, Acc));
    {CortexPid, terminate} ->
      ok
  end;
loop(Id, CortexPid, ActuatorName, {[], MemoryFaninPids}, Acc) ->
  neurev_actuator:ActuatorName(lists:reverse(Acc)),
  CortexPid ! {self(), sync},
  loop(Id, CortexPid, ActuatorName, {MemoryFaninPids, MemoryFaninPids}, []).

pts(Result) ->
  io:format("actuator:pts/1 -> ~p~n", [Result]).
