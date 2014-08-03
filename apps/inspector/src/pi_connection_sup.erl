-module(pi_connection_sup).
-author('mhald@mac.com').

-behaviour(supervisor).

-export([init/1,
         start_link/0,
         start_connection/0]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}} | {error, string()}.
-spec start_connection() -> supervisor:startchild_ret().

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

start_connection() ->
    supervisor:start_child(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init({}) -> 'ignore' | {'ok',{{'one_for_all',non_neg_integer(),non_neg_integer()} | {'one_for_one',non_neg_integer(),non_neg_integer()} | {'rest_for_one',non_neg_integer(),non_neg_integer()} | {'simple_one_for_one',non_neg_integer(),non_neg_integer()},[{_,{atom() | tuple(),atom(),'undefined' | [any()]},'permanent' | 'temporary' | 'transient','brutal_kill' | 'infinity' | non_neg_integer(),'supervisor' | 'worker','dynamic' | [atom() | tuple()]}]}}.
init({}) ->
    lager:debug("Starting pi_connection_sup listener"),
    {ok, {{simple_one_for_one, 5, 60},
            [{pi_connection_sup, {pi_connection, start_link, []}, temporary, 2000, worker, [pi_connection]}]
         }}.
