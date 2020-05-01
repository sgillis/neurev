-module(neurev_trainer).

-export([ go/1
        , go/4
        , loop/8
        ]).

-include("neurev.hrl").
-include_lib("kernel/include/logger.hrl").

-define(MAX_ATTEMPTS, 100).
-define(EVAL_LIMIT, inf).
-define(FITNESS_TARGET, inf).

go(Morphology) ->
  ?LOG_INFO("starting trainer"),
  go( Morphology
    , ?MAX_ATTEMPTS
    , ?EVAL_LIMIT
    , ?FITNESS_TARGET
    ).

go( Morphology
  , MaxAttempts
  , EvalLimit
  , FitnessTarget
  ) ->
  ?LOG_DEBUG("test"),
  Pid = spawn(neurev_trainer, loop, [ Morphology
                                    , FitnessTarget
                                    , {1, MaxAttempts}
                                    , {0, EvalLimit}
                                    , {0, best}
                                    , experimental
                                    , 0
                                    , 0
                                    ]),
  register(trainer, Pid),
  Pid.

loop( Morphology
    , FitnessTarget
    , {AttemptAcc, MaxAttempts}
    , {EvalAcc, EvalLimit}
    , {BestFitness, _BestG}
    , _ExpG
    , CAcc
    , TAcc
    ) when (AttemptAcc >= MaxAttempts) or
           (EvalAcc >= EvalLimit) or
           (BestFitness >= FitnessTarget) ->
  ?LOG_INFO("Morphology=~p", [Morphology]),
  ?LOG_INFO("EvalAcc=~p", [EvalAcc]),
  ?LOG_INFO("BestFitness=~p", [BestFitness]),
  unregister(trainer),
  case whereis(benchmarker) of
    undefined ->
      ?LOG_DEBUG("Benchmarker not found"),
      ok;
    Pid ->
      ?LOG_DEBUG("Sending info to benchmarker"),
      Pid ! {self(), BestFitness, EvalAcc, CAcc, TAcc}
  end;
loop( Morphology
    , FitnessTarget
    , {AttemptAcc, MaxAttempts}
    , {EvalAcc0, EvalLimit}
    , {BestFitness, BestG}
    , ExpG
    , CAcc0
    , TAcc0
    ) ->
  {ok, Genotype} = neurev_genotype:construct(
                     ExpG, Morphology),
  ExoPid = spawn(neurev_exoself, prep, [ExpG, Genotype]),
  receive
    {ExoPid, Fitness, Evals, Cycles, Time} ->
      EvalAcc = EvalAcc0 + Evals,
      CAcc = CAcc0 + Cycles,
      TAcc = TAcc0 + Time,
      case Fitness > BestFitness of
        true ->
          ?LOG_DEBUG("Writing genotype=~p with new best fitness=~p ~p",
                       [BestG, Fitness, Genotype]),
          neurev_genotype:write(BestG, Genotype),
          ?MODULE:loop( Morphology
                      , FitnessTarget
                      , {1, MaxAttempts}
                      , {EvalAcc, EvalLimit}
                      , {Fitness, BestG}
                      , ExpG
                      , CAcc
                      , TAcc
                      );
        false ->
          ?MODULE:loop( Morphology
                      , FitnessTarget
                      , {AttemptAcc + 1, MaxAttempts}
                      , {EvalAcc, EvalLimit}
                      , {BestFitness, BestG}
                      , ExpG
                      , CAcc
                      , TAcc
                      )
      end;
    terminate ->
      ?LOG_DEBUG("Trainer terminated"),
      neurev_genotype:print(BestG),
      ?LOG_DEBUG("Morphology=~p", [Morphology]),
      ?LOG_DEBUG("EvalAcc=~p", [EvalAcc0]),
      ?LOG_DEBUG("BestFitness=~p", [BestFitness])
  end.
