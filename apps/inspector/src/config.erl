-module(config).
-author('mhald@mac.com').

-export([get_env/2, get_env/1]).

-spec get_env(atom()) ->any().
-spec get_env(atom(), any()) -> any().

get_env(Param) -> get_env(Param, undefined).

get_env(Param, Default) ->
    case application:get_env(inspector, Param) of
        {ok, Val} -> Val;
        undefined -> Default
    end.
