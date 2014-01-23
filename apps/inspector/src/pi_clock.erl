-module(pi_clock).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, clock/0, timestamp/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	handle_info/2, terminate/2, code_change/3]).

-record(state, {
    tref = undefined :: undefined | timer:tref()
}).
-type state() :: #state{}.

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

%%======================== 
%% API
%%======================== 

-spec start_link() -> {ok, pid()}.
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() -> gen_server:call(?SERVER, stop).

-spec clock() -> integer().
clock() -> ets:lookup_element(?TABLE, gregorian_secs, 2).

-spec timestamp() -> string().
timestamp() -> ets:lookup_element(?TABLE, timestamp, 2).

%%======================== 
%% gen_server callbacks
%%======================== 

%% @private
-spec init([]) -> {ok, state()}.
init([]) ->
    ?TABLE = ets:new(?TABLE, [set, protected, named_table, {read_concurrency, true}]),
    true = update_time(),
    {ok, TRef} = timer:send_interval(1000, update),
    {ok, #state{tref=TRef}}.

%% @private
-spec handle_call(_, _, state())
    -> {reply, ignored, state()} | {stop, normal, stopped, state()}.
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @private
-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(_, state()) -> {noreply, state()}.
handle_info(update, State) ->
    true = update_time(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(_, _) -> ok.
terminate(_Reason, #state{tref=TRef}) ->
    {ok, cancel} = timer:cancel(TRef),
    true = ets:delete(?TABLE),
    ok.

%% @private
-spec code_change(_, state(), _) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%======================== 
%% Support functions
%%======================== 

%% @private
-spec update_time() -> true.
update_time() ->
    T   = {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    S   = calendar:datetime_to_gregorian_seconds(T),
    TS  = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~.10.0BZ",
            [Year, Month, Day, Hour, Minute, Second, 0])),
    ets:insert(?TABLE, [{gregorian_secs, S}, {timestamp , TS}]).
