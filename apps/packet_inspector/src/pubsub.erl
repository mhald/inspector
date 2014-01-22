-module(pubsub).

-behaviour(gen_server).

%% API
-export([start_link/0,
         publish/2,
         create_channel/1,
         safe_create_channel/1,
         account_channel/1,
         subscribe/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% TODO: store the state in an ETS table that can survive restarts
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-type channel() :: atom().
-type message() :: binary().

-spec publish(channel(), message()) -> ok.
-spec create_channel(channel()) -> ok.
-spec safe_create_channel(channel()) -> ok.
-spec subscribe(channel(), pid()) -> ok.
-spec account_channel(string()) -> binary().

publish(Channel, Message) -> gen_server:cast(?MODULE, {publish, Channel, Message}).
create_channel(Channel) -> gen_server:cast(?MODULE, {create_channel, Channel}).
safe_create_channel(Channel) -> gen_server:cast(?MODULE, {safe_create_channel, Channel}).
subscribe(Channel, Process) -> gen_server:cast(?MODULE, {subscribe, Channel, Process}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
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
-spec init([]) -> {ok, #state{}}.
init([]) ->
    create_channel('user login'),
    create_channel('user logout'),
    create_channel('live event'),
    {ok, #state{}}.

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
-spec handle_call(term(), pid(), term()) -> {reply, ok, #state{}}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({create_channel, Channel}, State) ->
    ets:new(channel_name(Channel), [bag, named_table, private, {keypos, 1}]),
    {noreply, State};

handle_cast({safe_create_channel, Channel}, State) ->
    case ets:info(channel_name(Channel)) of
        undefined -> ets:new(channel_name(Channel), [bag, named_table, private, {keypos, 1}]);
        _ -> ok
    end,
    {noreply, State};

handle_cast({subscribe, Channel, Process}, State) ->
    ets:insert(channel_name(Channel), {Process}),
    {noreply, State};

handle_cast({publish, Channel, Message}, State) ->
    lager:info("Publishing message to ~p ~p", [Channel, Message]),
    broadcast(Channel, Message, ets:tab2list(channel_name(Channel))),
    {noreply, State};

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
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
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
-spec terminate(string(), #state{}) -> ok.
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
-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

broadcast(Channel, _Message, []) ->
    lager:info("No listeners for channel ~p", [Channel]);
broadcast(Channel, Message, Listeners) ->
    [begin
                lager:info("Sending message to ~p", [Listener]),
                Listener ! {Channel, Message} 
     end || {Listener} <- Listeners].

channel_name(Channel) ->
    list_to_atom("pubsub_"++atom_to_list(Channel)).

account_channel(Account) -> list_to_atom(binary_to_list(iolist_to_binary([<<"traffic">>, $_, Account]))).
