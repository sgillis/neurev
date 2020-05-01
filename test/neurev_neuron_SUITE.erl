-module(neurev_neuron_SUITE).

 %% Test server callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ neuron_io/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("neurev.hrl").

%%--------------------------------------------------------------------
%% Common test callback functions
%%--------------------------------------------------------------------

all() ->
  [ neuron_io
  ].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(Case, Config) ->
  ?MODULE:Case({init, Config}).

end_per_testcase(Case, Config) ->
  ?MODULE:Case({'end', Config}).


%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

neuron_io({init, Config}) ->
  NeuronPid = setup_neuron(),
  [{neuron_pid, NeuronPid} | Config];
neuron_io({'end', Config}) ->
  NeuronPid = ?config(neuron_pid, Config),
  cleanup_neuron(NeuronPid),
  ok;
neuron_io(Config) ->
  NeuronPid = ?config(neuron_pid, Config),
  NeuronPid ! {self(), get_backup},
  InitialBackup =
    receive
      {NeuronPid, Id, MInputs} ->
        {Id, MInputs}
    end,
  ?assertEqual({ "neuron_id"
               , [ {pid1, [0.1, 0.2]}
                 , {pid2, [0.3, 0.4]}
                 , -0.2
                 ]
               }, InitialBackup),
  NeuronPid ! {pid2, forward, [0.3, 0.4]},
  NeuronPid ! {pid1, forward, [0.1, 0.2]},
  Output = receive
             {NeuronPid, forward, [Out]} ->
               Out
           end,
  ExpectedOutput =
    neurev_neuron:tanh(0.1 * 0.1 + 0.2 * 0.2 + 0.3 * 0.3 + 0.4 * 0.4 - 0.2),
  ?assertEqual(
     math:floor(ExpectedOutput * 1000000),
     math:floor(Output * 1000000)),
  ok.

setup_neuron() ->
  NeuronPid = neurev_neuron:gen(self(), node()),
  NeuronPid ! {self(), { "neuron_id"
                       , cortex_pid
                       , tanh
                       , [ {pid1, [0.1, 0.2]}
                         , {pid2, [0.3, 0.4]}
                         , -0.2
                         ]
                       , [self()]
                       }},
  NeuronPid.

cleanup_neuron(NeuronPid) ->
  NeuronPid ! {self(), terminate}.

