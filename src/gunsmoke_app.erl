%%%-------------------------------------------------------------------
%% @doc gunsmoke public API
%% @end
%%%-------------------------------------------------------------------

-module(gunsmoke_app).

-behaviour(application).

-export([ get_config/1
        , get_config/2
        , start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    gunsmoke_sup:start_link().

stop(_State) ->
    ok.

%%
%% @doc Get the relevant Application config
%%
-spec(get_config([Key :: atom()]) -> map()).
get_config(Keys) ->
    get_config(Keys, #{}).

-spec(get_config([Key :: atom()], Map :: map()) -> map()).
get_config(Keys, CfgMap) ->
    lists:foldl(
      fun(Key, Map) ->
              case application:get_env(gunsmoke, Key) of
                  {ok, Val} ->
                      maps:put(Key, Val, Map);
                      _ ->
                      throw({missing_config, Key})
              end
      end, CfgMap, Keys).

%% internal functions
