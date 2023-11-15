%%%-------------------------------------------------------------------
%% @doc gunsmoke public API
%% @end
%%%-------------------------------------------------------------------

-module(gunsmoke_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gunsmoke_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
