-module(neurev_actuator).

-export([ gen/2
        , prep/1
        , pts/2
        , xor_send_output/2
        ]).

-include("neurev.hrl").
-include_lib("kernel/include/logger.hrl").


%% API ---------------------------------------------------------------

gen(ExoPid, Node) ->
  spawn(Node, ?MODULE, prep, [ExoPid]).

prep(ExoPid) ->
  ?LOG_DEBUG("actuator=~p waiting for setup", [self()]),
  receive
    {ExoPid, {Id, CortexPid, Scape, ActuatorName, FaninPids}} ->
      ?LOG_DEBUG("actuator=~p setup received", [self()]),
      loop(Id, ExoPid, CortexPid, Scape, ActuatorName,
           {FaninPids, FaninPids}, [])
  end.

loop(Id, ExoPid, CortexPid, Scape, ActuatorName,
     {[FromPid | FaninPids], MemoryFaninPids}, Acc) ->
  receive
    {FromPid, forward, Input} ->
      loop(Id, ExoPid, CortexPid, Scape, ActuatorName,
           {FaninPids, MemoryFaninPids}, lists:append(Input, Acc));
    {CortexPid, terminate} ->
      ok
  end;
loop(Id, ExoPid, CortexPid, Scape, ActuatorName,
     {[], MemoryFaninPids}, Acc) ->
  {Fitness, EndFlag} = neurev_actuator:ActuatorName(lists:reverse(Acc), Scape),
  CortexPid ! {self(), sync, Fitness, EndFlag},
  loop(Id, ExoPid, CortexPid, Scape, ActuatorName,
       {MemoryFaninPids, MemoryFaninPids}, []).

pts(Result, _Scape) ->
  ?LOG_DEBUG("actuator:pts/1 -> ~p~n", [Result]),
  {1, 1}.

xor_send_output(Output, Scape) ->
  Scape ! {self(), action, Output},
  receive
    {Scape, Fitness, HaltFlag} ->
      {Fitness, HaltFlag}
  end.
