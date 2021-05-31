-module(neurev_rand).

-export([ generate_id/0
        , pick/1
        , seed/0
        , seed/1
        ]).

-spec generate_id() -> string().
generate_id() ->
  uuid:uuid_to_string(uuid:get_v4()).

-spec pick([any()]) -> any() | no_return.
pick([]) ->
  error(empty_list);
pick(List) ->
  lists:nth(rand:uniform(length(List)), List).

-spec seed() -> ok.
seed() ->
  seed(erlang:timestamp()).

-spec seed({integer(), integer(), integer()}) -> ok.
seed(Seed = {_, _, _}) ->
  _State = rand:seed(exs1024, Seed),
  ok.


%% Tests =======================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pick_test_() ->
  Xs = lists:seq(1, 10),
  [ ?_assertError(empty_list, pick([]))
  , ?_assert(lists:member(pick(Xs), Xs))
  ].

-endif.
