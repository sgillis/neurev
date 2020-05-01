-module(neurev_benchmark).

-export([ go/1
        , go/5
        , loop/9
        ]).

-include("neurev.hrl").
-include_lib("kernel/include/logger.hrl").

-define(MAX_ATTEMPTS, 5).
-define(EVAL_LIMIT, inf).
-define(FITNESS_TARGET, inf).
-define(TOT_RUNS, 100).
-define(MORPHOLOGY, xor_mimic).

go(Morphology) ->
  go(Morphology, ?TOT_RUNS).

go(Morphology, TotRuns) ->
  go(Morphology, ?MAX_ATTEMPTS, ?EVAL_LIMIT,
     ?FITNESS_TARGET, TotRuns).

go(Morphology, MaxAttempts, EvalLimit,
   FitnessTarget, TotRuns) ->
  Pid = spawn(neurev_benchmark, loop, [ Morphology
                                      , MaxAttempts
                                      , EvalLimit
                                      , FitnessTarget
                                      , TotRuns
                                      , []
                                      , []
                                      , []
                                      , []
                                      ]),
  register(benchmarker, Pid).

loop(Morphology, _MA, _EL, _FT, 0, FitnessAcc, EvalsAcc,
     CyclesAcc, TimeAcc) ->
  ?LOG_INFO("Benchmark results for morphology=~p~n"
            "fitness:~n"
            "  max=~p~n"
            "  min=~p~n"
            "  avg=~p~n"
            "  std=~p~n"
            "evals:~n"
            "  max=~p~n"
            "  min=~p~n"
            "  avg=~p~n"
            "  std=~p~n"
            "cycles:~n"
            "  max=~p~n"
            "  min=~p~n"
            "  avg=~p~n"
            "  std=~p~n"
            "time:~n"
            "  max=~p~n"
            "  min=~p~n"
            "  avg=~p~n"
            "  std=~p~n",
            [ Morphology
            , lists:max(FitnessAcc)
            , lists:min(FitnessAcc)
            , avg(FitnessAcc)
            , avg_std(FitnessAcc)
            , lists:max(EvalsAcc)
            , lists:min(EvalsAcc)
            , avg(EvalsAcc)
            , avg_std(EvalsAcc)
            , lists:max(CyclesAcc)
            , lists:min(CyclesAcc)
            , avg(CyclesAcc)
            , avg_std(CyclesAcc)
            , lists:max(TimeAcc)
            , lists:min(TimeAcc)
            , avg(TimeAcc)
            , avg_std(TimeAcc)
            ]);
loop(Morphology, MaxAttempts, EvalLimit, FitnessTarget,
     BenchmarkIndex, FitnessAcc, EvalsAcc, CyclesAcc, TimeAcc) ->
  TrainerPid = neurev_trainer:go(Morphology, MaxAttempts, EvalLimit,
                                 FitnessTarget),
  ?LOG_DEBUG("Trainer started: ~p", [TrainerPid]),
  receive
    {TrainerPid, Fitness, Evals, Cycles, Time} ->
      ?LOG_DEBUG("Received message from trainer ~p", [TrainerPid]),
      loop(Morphology, MaxAttempts, EvalLimit,
           FitnessTarget, BenchmarkIndex - 1, [Fitness | FitnessAcc],
           [Evals | EvalsAcc], [Cycles | CyclesAcc], [Time | TimeAcc]);
    terminate ->
      loop(Morphology, MaxAttempts, EvalLimit,
           FitnessTarget, 0, FitnessAcc, EvalsAcc, CyclesAcc, TimeAcc);
    Msg ->
      ?LOG_WARNING("Unexpected message ~p", [Msg])
  end.

avg(List) ->
  lists:sum(List) / length(List).

avg_std(List) ->
  Avg = avg(List),
  std(List, Avg, []).

std([Val | List], Avg, Acc) ->
  std(List, Avg, [math:pow(Avg - Val, 2) | Acc]);
std([], _Avg, Acc) ->
  Variance = lists:sum(Acc) / length(Acc),
  math:sqrt(Variance).
