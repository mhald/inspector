%%%-------------------------------------------------------------------
%%% @author Martin Hald <mhald@mac.com>
%%% @doc Packet Inspector Main Supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(pi_sup).
-author('mhald@mac.com').

-behaviour(supervisor).

-export([start_link/0, init/1]).

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc  Starts a new supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%-------------------------------------------------------------------
%% SUPERVISOR API
%%-------------------------------------------------------------------
%% @hidden
-spec init([]) -> {ok, {{one_for_one, 5, 60}, [supervisor:child_spec()]}}.
init([]) ->
  {ok, {{one_for_one, 5, 60}, [
	{pi_web, {pi_web, start_link, []}, permanent, 2000, worker, [pi_web]},
	{pi_clock, {pi_clock, start_link, []}, permanent, 2000, worker, [pi_clock]},
	{pi_connection_sup, {pi_connection_sup, start_link, []}, permanent, 2000, supervisor, [pi_connection_sup]},
	{pi_listener, {pi_listener, start_link, []}, permanent, 2000, worker, [pi_listener]},
	{pi_sessions, {pi_sessions, start_link, []}, permanent, 2000, worker, [pi_sessions]},
	{pi_client, {pi_client, start_link, []}, permanent, 2000, worker, [pi_client]},
	{pubsub, {pubsub, start_link, []}, permanent, 2000, worker, [pubsub]}
  ]}}.
