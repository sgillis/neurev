-module(neurev_genotype_SUITE).

 %% Test server callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ clone/1
        , create/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("neurev.hrl").

%%--------------------------------------------------------------------
%% Common test callback functions
%%--------------------------------------------------------------------

all() ->
  [ clone
  , create
  ].

init_per_suite(Config) ->
  LoggerPrimaryConf = logger:get_primary_config(),
  ok = logger:update_primary_config(#{level => debug}),
  neurev_polis:start(),
  [ {logger_primary_conf, LoggerPrimaryConf}
    | Config ].

end_per_suite(Config) ->
  neurev_polis:stop(),
  LoggerPrimaryConf = ?config(logger_primary_conf, Config),
  ok = logger:set_primary_config(LoggerPrimaryConf),
  ok.

init_per_testcase(Case, Config) ->
  ?MODULE:Case({init, Config}).

end_per_testcase(Case, Config) ->
  ?MODULE:Case({'end', Config}).


%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

clone({init, Conf}) ->
  Conf;
clone({'end', _Conf}) ->
  ok;
clone(_Conf) ->
  SpecieId = test,
  AgentId = test,
  CloneAgentId = test_clone,
  SpecCon = #constraint{morphology = xor_mimic},
  F = fun() ->
          neurev_genotype:construct_agent(SpecieId, AgentId, SpecCon),
          neurev_genotype:clone_agent(AgentId, CloneAgentId),
          neurev_genotype:log(AgentId),
          neurev_genotype:log(CloneAgentId),
          neurev_genotype:delete_agent(AgentId),
          neurev_genotype:delete_agent(CloneAgentId),
          ok
      end,
  ?assertEqual({atomic, ok}, mnesia:transaction(F)).

create({init, Conf}) ->
  Conf;
create({'end', _Conf}) ->
  ok;
create(_Conf) ->
  SpecieId = test,
  AgentId = test,
  SpecCon = #constraint{morphology = xor_mimic},
  F = fun() ->
          case neurev_genotype:read({agent, test}) of
            undefined ->
              neurev_genotype:construct_agent(SpecieId, AgentId, SpecCon),
              neurev_genotype:log(AgentId)
          end
      end,
  ?assertEqual({atomic, ok}, mnesia:transaction(F)).
