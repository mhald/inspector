-module(pi_listener).
-author('mhald@mac.com').

-behaviour(gen_nb_server).

-export([init/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         start_link/0]).

-export([sock_opts/0,
         new_connection/4]).

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() -> gen_nb_server:start_link(?MODULE, []).

-spec init(_, _) -> {ok, any()}. %% TODO: fix any
init([], State) ->
    Host = config:get_env(server_address, {"127.0.0.1", 8001}),
    lager:debug("Starting with ~p", [Host]),
    case gen_nb_server:add_listen_socket(Host, State) of
        {ok, State1} -> {ok, State1};
        Error        ->
            lager:error("Unable to listen to socket ~p", [Error]),
            {error, State}
    end.

-spec sock_opts() -> [term()].
sock_opts() ->
    [binary, {active, once}, {packet, 0}, {reuseaddr, true}].

-spec new_connection(_,_,_,_) -> {ok, any()}. %% TODO fix any and _
new_connection(_IpAddr, _Port, Sock, State) ->
    case pi_connection_sup:start_connection() of
      {ok, Pid} ->
        lager:debug("Bound TCP socket to new erlang process"),
        ok = gen_tcp:controlling_process(Sock, Pid),
        gen_server:call(Pid, {set_socket, Sock});
        %pi_connection:set_socket(Pid, Sock);
      Error ->
        lager:error("~p:new_connection/4 failed for ~p ~p: ~p ~n", [?MODULE, {_IpAddr, _Port}, Error]),
        gen_tcp:close(Sock)
    end,
    {ok, State}.

-spec handle_cast(Request::term(), State::term()) ->
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), timeout() | hibernate} |
        {stop, Reason :: term(), NewState :: term()}.
handle_cast(Request, State) ->
    lager:warning("Unhandled request ~p", [Request]),
    {noreply, State}.

-spec handle_call(Request::term(), From::{pid(), Tag::term()},
    State::term()) ->
    {reply, Reply::term(), NewState::term()} |
    {reply, Reply::term(), NewState::term(), timeout() | hibernate} |
    {noreply, NewState::term()} |
    {noreply, NewState::term(), timeout() | hibernate} |
    {stop, Reason::term(), Reply::term(), NewState::term()}.
handle_call(Request, Caller, State) ->
    lager:warning("Unhandled request ~p from ~p", [Request, Caller]),
    {reply, ignored, State}.
    %{ok, State}.

-spec handle_info(Info::term(), State::term()) ->
    {noreply, NewState::term()} |
    {noreply, NewState::term(), timeout() | hibernate} |
    {stop, Reason::term(), NewState::term()}.
handle_info({tcp, _Sock, _Data}, State) -> {noreply, State};
handle_info(Msg, State) ->
    lager:warning("Unhandled handle_info ~p", [Msg]),
    {noreply, State}.

-spec terminate(Reason::(normal | shutdown | {shutdown, term()} | term()), State::term()) -> ok | term().
terminate(_Reason, _State) -> ok. 

-spec code_change(OldVsn::(term() | {down, term()}), State::term(), Extra::term()) ->
    {ok, NewState::term()} | {error, Reason::term()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
