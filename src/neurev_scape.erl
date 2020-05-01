-module(neurev_scape).

-export([ gen/2
        , prep/1
        , xor_sim/1
        , xor_sim/3
        ]).

-include("neurev.hrl").
-include_lib("kernel/include/logger.hrl").

gen(ExoPid, Node) ->
  spawn(Node, ?MODULE, prep, [ExoPid]).

prep(ExoPid) ->
  ?LOG_DEBUG("scape=~p waiting for setup", [self()]),
  receive
    {ExoPid, Name} ->
      ?LOG_DEBUG("scape=~p setup received", [self()]),
      neurev_scape:Name(ExoPid)
  end.

xor_sim(ExoPid) ->
  Xor = [ {[-1, -1], [-1]}
        , {[ 1, -1], [ 1]}
        , {[-1,  1], [ 1]}
        , {[ 1,  1], [-1]}
        ],
  xor_sim(ExoPid, {Xor, Xor}, 0).

xor_sim(ExoPid, {[{Input, CorrectOutput} | Xor], MXor}, ErrAcc) ->
  receive
    {From, sense} ->
      From ! {self(), percept, Input},
      xor_sim(ExoPid, {[{Input, CorrectOutput} | Xor], MXor}, ErrAcc);
    {From, action, Output} ->
      Error = list_compare(Output, CorrectOutput, 0),
      case Xor of
        [] ->
          Mse = math:sqrt(ErrAcc + Error),
          Fitness = 1 / (Mse + 0.00001),
          From ! {self(), Fitness, 1},
          xor_sim(ExoPid, {MXor, MXor}, 0);
        _ ->
          From ! {self(), 0, 0},
          xor_sim(ExoPid, {Xor, MXor}, ErrAcc + Error)
      end;
    {ExoPid, terminate} ->
      ok
  end.

list_compare([X | List1], [Y | List2], ErrorAcc) ->
  list_compare(List1, List2, ErrorAcc + math:pow(X - Y, 2));
list_compare([], [], ErrorAcc) ->
  math:sqrt(ErrorAcc).
