%%%-------------------------------------------------------------------
%% @doc neurev public API
%% @end
%%%-------------------------------------------------------------------

-module(neurev_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  ok = logger:update_primary_config(#{level => debug}),
  neurev_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================