-module(neurev_simple_neuron).

-export([ create/0
        , neuron/3
        , sensor/1
        , actuator/1
        , cortex/3
        ]).

-include_lib("neurev/include/neurev.hrl").


%% API ---------------------------------------------------------------

create() ->
  Weights = [random(), random(), random()],
  N_Pid = spawn(?MODULE, neuron, [Weights, undefined, undefined]),
  S_Pid = spawn(?MODULE, sensor, [N_Pid]),
  A_Pid = spawn(?MODULE, actuator, [N_Pid]),
  N_Pid ! {init, S_Pid, A_Pid},
  register(cortex, spawn(?MODULE, cortex, [S_Pid, N_Pid, A_Pid])).

neuron(Weights, S_Pid, A_Pid) ->
  receive
    {S_Pid, forward, Input} ->
      io:format("***Thinking***~nInput: ~p~nWeights: ~p~n",
                [Input, Weights]),
      DotProduct = dot(Input, Weights, 0),
      Output = [math:tanh(DotProduct)],
      A_Pid ! {self(), forward, Output},
      neuron(Weights, S_Pid, A_Pid);
    {init, New_SPid, New_APid} ->
      io:format("init neuron"),
      neuron(Weights, New_SPid, New_APid);
    terminate ->
      ok
  end.

sensor(N_Pid) ->
  receive
    sync ->
      SensorySignal = [random(), random()],
      io:format("***Sensing***:~nSignal from env: ~p~n", [SensorySignal]),
      N_Pid ! {self(), forward, SensorySignal},
      sensor(N_Pid);
    terminate ->
      ok
  end.

actuator(N_Pid) ->
  receive
    {N_Pid, forward, ControlSignal} ->
      pts(ControlSignal),
      actuator(N_Pid);
    terminate ->
      ok
  end.

cortex(SensorPid, NeuronPid, ActuatorPid) ->
  io:format("SensorPid: ~p~nNeuronPid: ~p~nActuatorPid: ~p~n", [SensorPid, NeuronPid, ActuatorPid]),
  receive
    sense_think_act ->
      SensorPid ! sync,
      cortex(SensorPid, NeuronPid, ActuatorPid);
    terminate ->
      SensorPid ! terminate,
      NeuronPid ! terminate,
      ActuatorPid ! terminate,
      ok
  end.

%% Internal functions ------------------------------------------------

dot([I | Input], [W | Weights], Acc) ->
  dot(Input, Weights, I * W + Acc);
dot([], [Bias], Acc) ->
  Acc + Bias.

random() ->
  rand:uniform() - 0.5.

pts(ControlSignal) ->
  io:format("***Acting***: ~p~n", [ControlSignal]).
