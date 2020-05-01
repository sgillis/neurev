-module(neurev_neuron).

-export([ gen/2
        , prep/1
        , loop/7
        , tanh/1
        ]).

-include("neurev.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DELTA_MULTIPLIER, math:pi() * 2).
-define(SAT_LIMIT, math:pi() * 2).


%% API ---------------------------------------------------------------

gen(ExoPid, Node) ->
  spawn(Node, ?MODULE, prep, [ExoPid]).

prep(ExoPid) ->
  rand:seed(exs1024, erlang:timestamp()),
  receive
    {ExoPid, {Id, CortexPid, ActivationFunction, InputPids, OutputPids}} ->
      loop(Id, ExoPid, CortexPid, ActivationFunction, {InputPids, InputPids},
           OutputPids, 0)
  end.

loop(Id, ExoPid, CortexPid, ActivationFunction,
     {[{InputPid, Weights} | Inputs], MInputs}, OutputPids, Acc) ->
  receive
    {InputPid, forward, Input} ->
      Result = dot(Input, Weights, 0),
      loop(Id, ExoPid, CortexPid, ActivationFunction, {Inputs, MInputs},
           OutputPids, Result + Acc);
    {ExoPid, weight_backup} ->
      put(weights, MInputs),
      loop(Id, ExoPid, CortexPid, ActivationFunction,
           {[{InputPid, Weights} | Inputs], MInputs},
           OutputPids, Acc);
    {ExoPid, weight_restore} ->
      RInputs = get(weights),
      loop(Id, ExoPid, CortexPid, ActivationFunction, {RInputs, RInputs},
           OutputPids, Acc);
    {ExoPid, weight_perturb} ->
      PInputs = perturb_inputs(MInputs),
      loop(Id, ExoPid, CortexPid, ActivationFunction, {PInputs, PInputs},
           OutputPids, Acc);
    {ExoPid, get_backup} ->
      ExoPid ! {self(), Id, MInputs},
      loop(Id, ExoPid, CortexPid, ActivationFunction,
           {[{InputPid, Weights} | Inputs], MInputs}, OutputPids, Acc);
    {ExoPid, terminate} ->
      ok
  end;
loop(Id, ExoPid, CortexPid, ActivationFunction, {[Bias], MInputs}, OutputPids,
     Acc) ->
  Output = neurev_neuron:ActivationFunction(Acc + Bias),
  [OutputPid ! {self(), forward, [Output]} || OutputPid <- OutputPids],
  loop(Id, ExoPid, CortexPid, ActivationFunction, {MInputs, MInputs},
       OutputPids, 0);
loop(Id, ExoPid, CortexPid, ActivationFunction, {[], MInputs}, OutputPids,
     Acc) ->
  Output = neurev_neuron:ActivationFunction(Acc),
  [OutputPid ! {self(), forward, [Output]} || OutputPid <- OutputPids],
  loop(Id, ExoPid, CortexPid, ActivationFunction, {MInputs, MInputs},
       OutputPids, 0).

tanh(Val) ->
  math:tanh(Val).


%% Internal functions ------------------------------------------------

dot([I | Input], [W | Weights], Acc) ->
  dot(Input, Weights, I * W + Acc);
dot([], [], Acc) ->
  Acc.

perturb_inputs(Inputs) ->
  TotWeights = lists:sum([length(Weights) || {_InputPid, Weights} <- Inputs]),
  MP = 1 / math:sqrt(TotWeights),
  perturb_inputs(MP, Inputs, []).

perturb_inputs(MP, [{InputPid, Weights} | Inputs], Acc) ->
  UWeights = perturb_weights(MP, Weights, []),
  perturb_inputs(MP, Inputs, [{InputPid, UWeights} | Acc]);
perturb_inputs(MP, [Bias], Acc) ->
  UBias = case rand:uniform() < MP of
            true ->
              sat((rand:uniform() - 0.5) * ?DELTA_MULTIPLIER + Bias,
                  -?SAT_LIMIT, ?SAT_LIMIT);
            false ->
              Bias
          end,
  lists:reverse([UBias | Acc]);
perturb_inputs(_MP, [], Acc) ->
  lists:reverse(Acc).

perturb_weights(MP, [W | Weights], Acc) ->
  U_W = case rand:uniform() < MP of
          true ->
            sat((rand:uniform() - 0.5) * ?DELTA_MULTIPLIER + W,
                -?SAT_LIMIT, ?SAT_LIMIT);
          false ->
            W
        end,
  perturb_weights(MP, Weights, [U_W | Acc]);
perturb_weights(_MP, [], Acc) ->
  lists:reverse(Acc).

sat(Val, Min, Max) ->
  if
    Val < Min ->
      Min;
    Val > Max ->
      Max;
    true ->
      Val
  end.
