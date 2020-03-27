-module(neurev_genotype).

-include("neurev.hrl").

-export([ construct/3
        , construct/4
        , write/2
        , read/1
        , print/1
        , table/0
        ]).

-define(TABLE, genotypes).

%% API ---------------------------------------------------------------

construct( GenotypeName
         , SensorName
         , ActuatorName
         , HiddenLayerDensities) ->
  Genotype = construct(SensorName, ActuatorName, HiddenLayerDensities),
  ok = write(GenotypeName, Genotype),
  {ok, Genotype}.

construct(SensorName, ActuatorName, HiddenLayerDensities) ->
  S = create_sensor(SensorName),
  A = create_actuator(ActuatorName),
  OutputVectorLength = A#actuator.vector_length,
  LayerDensities = lists:append(HiddenLayerDensities,
                                [OutputVectorLength]),
  CortexId = {neurev_cortex, generate_id()},
  Neurons = create_neurolayers(CortexId, S, A, LayerDensities),
  [InputLayer | _] = Neurons,
  [OutputLayer | _] = lists:reverse(Neurons),
  InputNeuronIds = [N#neuron.id || N <- InputLayer],
  OutputNeuronIds = [N#neuron.id || N <- OutputLayer],
  NeuronIds = [N#neuron.id || N <- lists:flatten(Neurons)],
  Sensor = S#sensor{ cortex_id = CortexId
                   , fanout_ids = InputNeuronIds
                   },
  Actuator = A#actuator{ cortex_id = CortexId
                       , fanin_ids = OutputNeuronIds
                       },
  Cortex = create_cortex(CortexId, [S#sensor.id], [A#actuator.id], NeuronIds),
  #genotype{ id = generate_id()
           , cortex = Cortex
           , sensor = Sensor
           , actuator = Actuator
           , neurons = lists:flatten(Neurons)
           }.

write(GenotypeName, Genotype) ->
  ets:insert(?TABLE, {GenotypeName, Genotype}),
  ok.

read(GenotypeName) ->
  [{GenotypeName, Genotype}] = ets:lookup(?TABLE, GenotypeName),
  Genotype.

print(GenotypeName) ->
  Genotype = read(GenotypeName),
  {ok, Defs} = pp_record:read(?MODULE),
  io:format("~s~n", [pp_record:print(Genotype, Defs)]).

table() ->
  ?TABLE.

%% Internal functions ------------------------------------------------

create_sensor(SensorName) ->
  case SensorName of
    rng ->
      #sensor{ id = {sensor, generate_id()}
             , name = rng
             , vector_length = 2
             };
    _ ->
      error({unsupported_sensor_name, SensorName})
  end.

create_actuator(ActuatorName) ->
  case ActuatorName of
    pts ->
      #actuator{ id = {actuator, generate_id()}
               , name = pts
               , vector_length = 1
               };
    _ ->
      error({unsupported_actuator_name, ActuatorName})
  end.

create_neurolayers(CortexId, Sensor, Actuator, LayerDensities) ->
  InputIds = [{Sensor#sensor.id, Sensor#sensor.vector_length}],
  TotalLayers = length(LayerDensities),
  [FirstLayerNeurons | NextLayerDensities] = LayerDensities,
  NeuronIds = [{neuron, {1, Id}}
               || Id <- generate_ids(FirstLayerNeurons, [])],
  create_neurolayers(CortexId,
                     Actuator#actuator.id,
                     1,
                     TotalLayers,
                     InputIds,
                     NeuronIds,
                     NextLayerDensities,
                     []).

create_neurolayers(CortexId, ActuatorId, LayerIndex, TotalLayers, InputIds,
                   NeuronIds, [NextLayerDensity | LayerDensities], Acc) ->
  OutputIds = [{neuron, {LayerIndex + 1, Id}} ||
                Id <- generate_ids(NextLayerDensity, [])],
  LayerNeurons =
    create_neurolayer(CortexId, InputIds, NeuronIds, OutputIds, []),
  NextInputIds = [{NeuronId, 1} || NeuronId <- NeuronIds],
  create_neurolayers(CortexId,
                     ActuatorId,
                     LayerIndex + 1,
                     TotalLayers,
                     NextInputIds,
                     OutputIds,
                     LayerDensities,
                     [LayerNeurons | Acc]);
create_neurolayers(CortexId, ActuatorId, TotalLayers, TotalLayers, InputIds,
                   NeuronIds, [], Acc) ->
  OutputIds = [ActuatorId],
  LayerNeurons =
    create_neurolayer(CortexId, InputIds, NeuronIds, OutputIds, []),
  lists:reverse([LayerNeurons | Acc]).

create_neurolayer(CortexId, InputIds, [Id | NeuronIds], OutputIds, Acc) ->
  Neuron = create_neuron(InputIds, Id, CortexId, OutputIds),
  create_neurolayer(CortexId, InputIds, NeuronIds, OutputIds, [Neuron | Acc]);
create_neurolayer(_CortexId, _InputIds, [], _OutputIds, Acc) ->
  Acc.

create_neuron(InputIds, Id, CortexId, OutputIds) ->
  ProperInputIds = create_neural_input(InputIds, []),
  #neuron{ id = Id
         , cortex_id = CortexId
         , activation_function = tanh
         , input = ProperInputIds
         , output = OutputIds
         }.

create_neural_input([{InputId, InputVectorLength} | InputIds], Acc) ->
  Weights = create_neural_weights(InputVectorLength, []),
  create_neural_input(InputIds, [{InputId, Weights} | Acc]);
create_neural_input([], Acc) ->
  lists:reverse([{bias, rand:uniform() - 0.5} | Acc]).

create_neural_weights(0, Acc) ->
  Acc;
create_neural_weights(Index, Acc) ->
  W = rand:uniform() - 0.5,
  create_neural_weights(Index - 1, [W | Acc]).

generate_ids(0, Acc) ->
  Acc;
generate_ids(Index, Acc) ->
  Id = generate_id(),
  generate_ids(Index - 1, [Id | Acc]).

generate_id() ->
  erlang:phash2(erlang:timestamp()).

create_cortex(CortexId, SensorIds, ActuatorIds, NeuronIds) ->
  #cortex{ id = CortexId
         , sensor_ids = SensorIds
         , actuator_ids = ActuatorIds
         , neuron_ids = NeuronIds
         }.
