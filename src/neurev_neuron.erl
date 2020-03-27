-module(neurev_neuron).

-export([ gen/2
        , loop/1
        , tanh/1
        ]).

-include("neurev.hrl").


%% API ---------------------------------------------------------------

gen(ExoSelfPid, Node) ->
  spawn(Node, ?MODULE, loop, [ExoSelfPid]).

loop(ExoSelfPid) ->
  receive
    {ExoSelfPid, {Id, CortexPid, ActivationFunction, InputPids, OutputPids}} ->
      loop(Id, CortexPid, ActivationFunction, {InputPids, InputPids}, OutputPids, 0)
  end.

loop(Id, CortexPid, ActivationFunction,
     {[{InputPid, Weights} | InputPids], MemoryInputPids},
     OutputPids, Acc) ->
  receive
    {InputPid, forward, Input} ->
      Result = dot(Input, Weights, 0),
      loop(Id, CortexPid, ActivationFunction, {InputPids, MemoryInputPids},
           OutputPids, Result + Acc);
    {CortexPid, get_backup} ->
      CortexPid ! {self(), Id, MemoryInputPids},
      loop(Id, CortexPid, ActivationFunction,
           {[{InputPid, Weights} | InputPids], MemoryInputPids},
           OutputPids, Acc);
    {CortexPid, terminate} ->
      ok
  end;
loop(Id, CortexPid, ActivationFunction, {[Bias], MemoryInputPids},
     OutputPids, Acc) ->
  Output = neurev_neuron:ActivationFunction(Acc + Bias),
  [OutputPid ! {self(), forward, [Output]} || OutputPid <- OutputPids],
  loop(Id, CortexPid, ActivationFunction,
       {MemoryInputPids, MemoryInputPids}, OutputPids, 0);
loop(Id, CortexPid, ActivationFunction, {[], MemoryInputPids},
     OutputPids, Acc) ->
  Output = neurev_neuron:ActivationFunction(Acc),
  [OutputPid ! {self(), forward, [Output]} || OutputPid <- OutputPids],
  loop(Id, CortexPid, ActivationFunction, {MemoryInputPids, MemoryInputPids},
       OutputPids, 0).

tanh(Val) ->
  math:tanh(Val).


%% Internal functions ------------------------------------------------

dot([I | Input], [W | Weights], Acc) ->
  dot(Input, Weights, I * W + Acc);
dot([], [], Acc) ->
  Acc.
