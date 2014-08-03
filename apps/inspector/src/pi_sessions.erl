-module(pi_sessions).

-behaviour(gen_server).

%% API
-export([start_link/0,
    add/2,
    remove/2,
    update/3,
    get_sessions/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("inspector.hrl").

-record(state, {
        ets :: integer()
    }).

-record(sess_rec, {
        key :: term(),
        server :: atom()
    }).

%%%===================================================================
%%% API
%%%===================================================================

-spec update(atom(), atom(), [binary()]) -> ok.
-spec add(atom(), [binary()]) -> ok.
-spec remove(atom(), [binary()]) -> ok.
update(Action, Server, Sessions) -> gen_server:cast(?MODULE, {update, Action, Server, Sessions}).
add(Server, Accounts) -> gen_server:cast(?MODULE, {add, Server, Accounts}).
remove(Server, Accounts) -> gen_server:cast(?MODULE, {remove, Server, Accounts}).

-spec get_sessions() -> [#account{}].
get_sessions() -> gen_server:call(?MODULE, {get_sessions}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(_) -> {ok, #state{}}.
init([]) ->
    ETS = ets:new(sessions, [bag, {keypos, 2}]),
    put(database_ref, ETS),
    {ok, #state{ets=ETS}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::{pid(), Tag::term()},
    State::term()) ->
    {reply, Reply::term(), NewState::term()} |
    {reply, Reply::term(), NewState::term(), timeout() | hibernate} |
    {noreply, NewState::term()} |
    {noreply, NewState::term(), timeout() | hibernate} |
    {stop, Reason::term(), Reply::term(), NewState::term()}.
handle_call({get_sessions}, _Caller, #state{ets=ETS} = State) ->
    Reply = [Account || #sess_rec{key={_,Account}} <- ets:tab2list(ETS)],
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request::term(), State::term()) ->
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), timeout() | hibernate} |
        {stop, Reason :: term(), NewState :: term()}.
handle_cast({update, Action, Server, Sessions}, #state{ets=ETS} = State) ->
    [begin
                Key = {Server, Session},
                case Action of
                    add -> ets:insert(ETS, #sess_rec{key=Key, server=Server});
                    remove -> ets:delete(ETS, Key)
                end
     end || Session <- Sessions],
    {noreply, State};
handle_cast({add, Server, Accounts}, #state{ets=ETS} = State) ->
    [begin
                Key = {Server, Account},
                ets:insert(ETS, #sess_rec{key=Key, server=Server})
     end || Account <- Accounts],
    {noreply, State};
handle_cast({remove, Server, Accounts}, #state{ets=ETS} = State) ->
    [begin
                Key = {Server, Account},
                ets:delete(ETS, Key)
     end || Account <- Accounts],
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info::term(), State::term()) ->
    {noreply, NewState::term()} |
    {noreply, NewState::term(), timeout() | hibernate} |
    {stop, Reason::term(), NewState::term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::(normal | shutdown | {shutdown, term()} | term()), State::term()) -> ok | term().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn::(term() | {down, term()}), State::term(), Extra::term()) -> {ok, NewState::term()} | {error, Reason::term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

