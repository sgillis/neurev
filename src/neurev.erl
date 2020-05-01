-module(neurev).

-type id() :: string().
-type neuron_input() :: [{id(), float()} | {bias, float()}].
-type neuron_pid_input() :: [{pid(), float()} | {bias, float()}].
-type scape() :: any().

-export_type([ id/0
             , neuron_input/0
             , neuron_pid_input/0
             , scape/0
             ]).
