-module(neurev).

-type id() :: integer().
-type modfun() :: {atom(), atom()}.
-type neuron_input() :: [{id(), float()} | {bias, float()}].
-type neuron_pid_input() :: [{pid(), float()} | {bias, float()}].

-export_type([ id/0
             , modfun/0
             , neuron_input/0
             , neuron_pid_input/0
             ]).
