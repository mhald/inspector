%%%-------------------------------------------------------------------
%%% @author Martin Hald <mhald@mac.com>
%%% @doc Packet Inspector
%%% @end
%%%-------------------------------------------------------------------
-module(packet_inspector).
-author('mhald@mac.com').
-vsn('0.1').

-behaviour(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).

%%-------------------------------------------------------------------
%% ADMIN API
%%-------------------------------------------------------------------
%% @doc Starts the application
-spec start() -> ok | {error, {already_started, ?MODULE}}.
start() ->
  io:format("Starting cowboy~n"),
	application:start(cowboy),
	application:start(?MODULE).

%% @doc Stops the application
-spec stop() -> ok.
stop() -> application:stop(?MODULE).

%%-------------------------------------------------------------------
%% BEHAVIOUR CALLBACKS
%%-------------------------------------------------------------------
%% @private
-spec start(any(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    ets_buffer:create(history, ring, 50),
    pi_sup:start_link().

%% @private
-spec stop(any()) -> no_return().
stop(_State) ->
    ets_buffer:delete(history),
    _ = lager:critical("Application ~p stopped. Halting!", [?MODULE]),
    erlang:halt(1).
