-module(pi_client).
-author('mhald@mac.com').

-behavior(gen_fsm).

-export([init/1,
         terminate/3,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         start_link/1]).

-export([send_packet/3,
         send_packet/5,
         receive_packet/3,
         receive_packet/5,
         http_request/4,
         http_response/5,
         add_sessions/1,
         remove_sessions/1,
         encode/1,
         is_tracing_on/0]).

-export([
        connected/2,
        disconnected/2
    ]).

-record(state, {
        socket :: pid(),
        host :: string(),
        port :: integer(),
        sessions = [] :: list(),
        queue = [] :: list(),
        account_callback_mod :: atom(),
        account_callback_fun :: atom()
    }).

-define(IS_TRACING_ON, true).

-spec start_link([any()]) -> ok.
start_link(Params) -> gen_fsm:start_link({local, ?MODULE}, ?MODULE, Params, []).

-type account() :: string().
-type encoding() :: binary().
-type data() :: string().

-type url() :: string() | binary().
-type headers() :: [{header_name(), header_value()}].
-type header_name() :: string() | binary().
-type header_value() :: string() | binary().

-type parameters() :: [{parameter_name(), parameter_value()}].
-type parameter_name() :: string() | binary().
-type parameter_value() :: string() | binary().

-type userdata() :: {username(), userid()}.
-type username() :: string().
-type userid() :: string().

-type code() :: non_neg_integer().
-type body() :: string() | binary().

-spec receive_packet(account()|undefined, encoding(), data()) -> ok.
receive_packet(Account, _, _) when is_atom(Account) -> ok;
receive_packet(Account, Encoding, Data) -> receive_packet(Account, Encoding, undefined, undefined, Data).
receive_packet(Account, Encoding, Sender, Receiver, Data) when is_list(Account) -> 
    gen_fsm:send_event(?MODULE, {receive_packet, Account, Encoding, Sender, Receiver, Data}).

-spec send_packet(account()|undefined, encoding(), data()) -> ok.
send_packet(undefined, _, _) -> ok;
send_packet(Account, Encoding, Data) -> send_packet(Account, Encoding, undefined, undefined, Data).
send_packet(Account, Encoding, Sender, Reciever, Data) when is_list(Account) ->
    gen_fsm:send_event(?MODULE, {send_packet, Account, Encoding, Sender, Reciever, Data}).

-spec http_request(account(), url(), headers(), parameters()) -> ok.
http_request(undefined, _Url, _Headers, _Parameters) -> ok;
http_request(Account, Url, Headers, Parameters) when is_list(Account) -> gen_fsm:send_event(?MODULE, {http_request, Account, Url, Headers, Parameters}).

-spec http_response(account(), url(), code(), headers(), body()) -> ok.
http_response(undefined, _Url, _Code, _Headers, _Body) -> ok;
http_response(Account, Url, Code, Headers, Body) when is_list(Account) -> gen_fsm:send_event(?MODULE, {http_response, Account, Url, Code, Headers, Body}).

-spec add_sessions([userdata()]) -> ok.
add_sessions(Accounts) -> gen_fsm:send_event(?MODULE, {add_sessions, Accounts}).

-spec remove_sessions([userid()]) -> ok.
remove_sessions(Accounts) -> gen_fsm:send_event(?MODULE, {remove_sessions, Accounts}).

-spec init([any()]) -> {ok, #state{}}.
init(Params) ->
    %% TODO: exit with error if they parameters are not given
    {Mod, Fun} = proplists:get_value(account_info_callback, Params),
    {Host, Port} = proplists:get_value(host, Params),
    {ok, disconnected, #state{host=Host, port=Port, account_callback_mod=Mod, account_callback_fun=Fun}}.

-spec encode(Request::term()) -> {pi_packet, string()}.
encode({http_request, Account, Url, Headers, Parameters}) ->
    Json = jiffy:encode({[
                {<<"traffic">>, {[
                            {<<"account">>, as_binary(Account)},
                            {<<"direction">>, <<"received">>},
                            {<<"encoding">>, <<"http">>},
                            {<<"timestamp">>, now_for_timestamp()},
                            {<<"url">>, as_binary(Url)},
                            {<<"timestamp">>, now_for_timestamp()}, 
                            {<<"headers">>, proplist_to_json(Headers)},
                            {<<"parameters">>, proplist_to_json(Parameters)}
                        ]}
                }]
        }),
    {pi_packet, Json};
encode({http_response, Account, Url, Code, Headers, Body}) ->
    Json = jiffy:encode({[
                {<<"traffic">>, {[
                            {<<"account">>, as_binary(Account)},
                            {<<"direction">>, <<"sent">>},
                            {<<"encoding">>, <<"http">>},
                            {<<"timestamp">>, now_for_timestamp()},
                            {<<"url">>, as_binary(Url)},
                            {<<"code">>, Code},
                            {<<"headers">>, proplist_to_json(Headers)},
                            {<<"body">>, as_binary(Body)}
                        ]}
                }]
        }),
    {pi_packet, Json};
%% Sessions are {username, userid}
encode({active_sessions, Sessions}) ->
    Json = jiffy:encode({[
                {<<"session">>, {[
                    {<<"action">>, <<"add">>},
                    {<<"sessions">>,
                            [{[{<<"username">>, as_binary(Username)},
                               {<<"userid">>, as_binary(UserID)}] ++ avatar(Avatar)
                             }
                            || {Username, UserID, Avatar} <- Sessions]
                    }
                ]}
            }]
        }),
    {pi_packet, Json};
encode({sessions, Direction, Sessions}) ->
    lager:debug("Sessions ~p", [Sessions]),
    Json = jiffy:encode({[
                {<<"session">>, {[
                    {<<"action">>, list_to_binary(atom_to_list(Direction))},
                    {<<"server">>, list_to_binary(atom_to_list(node()))},
                    {<<"sessions">>, 
                            [{[{<<"username">>, as_binary(Username)},
                               {<<"userid">>, as_binary(UserID)}] ++ avatar(Avatar)
                             }
                            || {Username, UserID, Avatar} <- Sessions]
                    }
                ]}
            }]
        }),
    {pi_packet, Json};
encode(Other) ->
    lager:warning("Unknown encoding requested for unknown packet type ~p", [Other]),
    unknown_packet.

avatar(undefined) -> [];
avatar(Link) -> {<<"avatar">>, as_binary(Link)}.

-spec connected(Request::term(), State::term()) ->
    {next_state, NextStateName::atom(), NewState::term()} |
    {stop, Reason::term(), NewState::term()}.
connected({send_packet, Account, Encoding, Sender, Reciever, Text}, #state{socket=Socket, queue=[]} = State) ->
    Json = standard_pi_account_packet(<<"data_packet">>, Account, [
            {<<"direction">>, <<"sent">>},
            {<<"encoding">>, Encoding},
            {<<"data">>, list_to_binary(Text)}
        ], State),
    ok = gen_tcp:send(Socket, [Json, $\n]),
    {next_state, connected, State};
connected({receive_packet, Account, Encoding, Sender, Receiver, Text}, #state{socket=Socket, queue=[]} = State) ->
    Extra = case [Sender, Receiver] of
        [undefined, _] -> [];
        [_, undefined] -> [];
        [Sender, Receiver] -> 
            [{<<"sender">>, Sender}, {<<"receiver">>, Receiver}, {<<"on-behalf-of">>, true}]
    end,
    Json = standard_pi_account_packet(<<"data_packet">>, Account, [
            {<<"direction">>, <<"received">>},
            {<<"encoding">>, Encoding},
            {<<"data">>, list_to_binary(Text)}
        ] ++ Extra, State),
    ok = gen_tcp:send(Socket, [Json, $\n]),
    {next_state, connected, State};
connected({add_sessions, Accounts}, #state{socket=Socket} = State) ->
    Accounts_Data = [{get_account_data(Account, State)} || Account <- Accounts],
    Text = pi_util:standard_pi_general_packet(<<"add_sessions">>, [
            {<<"server">>, list_to_binary(atom_to_list(node()))},
            {<<"accounts">>, Accounts_Data}
        ]),
    ok = gen_tcp:send(Socket, [Text, $\n]),
    {next_state, connected, State#state{}};
connected({remove_sessions, Accounts}, #state{socket=Socket} = State) ->
    Accounts_Data = [{get_account_data(Account, State)} || Account <- Accounts],
    Text = pi_util:standard_pi_general_packet(<<"remove_sessions">>, [
            {<<"server">>, list_to_binary(atom_to_list(node()))},
            {<<"accounts">>, Accounts_Data}
        ]),
    ok = gen_tcp:send(Socket, [Text, $\n]),
    {next_state, connected, State#state{}};
connected({http_request, Account, Url, Headers, Parameters}, #state{socket=Socket, queue=[]} = State) ->
    Json = standard_pi_account_packet(<<"http_request">>, Account, [
            {<<"direction">>, <<"received">>},
            {<<"encoding">>, <<"http">>},
            {<<"timestamp">>, now_for_timestamp()},
            {<<"url">>, as_binary(Url)},
            {<<"headers">>, proplist_to_json(Headers)},
            {<<"parameters">>, proplist_to_json(Parameters)}
        ], State),
    ok = gen_tcp:send(Socket, [Json, $\n]),
    {next_state, connected, State};
connected({http_response, Account, Url, Code, Headers, Body}, #state{socket=Socket, queue=[]} = State) ->
    Json = standard_pi_account_packet(<<"http_response">>, Account, [
            {<<"direction">>, <<"sent">>},
            {<<"encoding">>, <<"http">>},
            {<<"timestamp">>, now_for_timestamp()},
            {<<"url">>, as_binary(Url)},
            {<<"code">>, Code},
            {<<"headers">>, proplist_to_json(Headers)},
            {<<"body">>, as_binary(Body)}
        ], State),
    ok = gen_tcp:send(Socket, [Json, $\n]),
    {next_state, connected, State}.

-spec disconnected(Request::term(), State::term()) ->
    {next_state, NextStateName::atom(), NewState::term()} |
    {stop, Reason::term(), NewState::term()}.
disconnected(unknown_packet, State) -> {next_data, disconnected, State};
disconnected({pi_packet, Text}, State) -> connect_and_process({pi_packet, Text}, State);
disconnected({send_packet, Account, Encoding, Text}, State) -> connect_and_process({send_packet, Account, Encoding, Text}, State);
disconnected({receive_packet, Account, Encoding, Text}, State) -> connect_and_process({received_packet, Account, Encoding, Text}, State);
disconnected({add_sessions, Text}, State) -> connect_and_process({pi_packet, Text}, State);
disconnected({remove_sessions, Text}, State) -> connect_and_process({pi_packet, Text}, State);
disconnected(Other, State) ->
    lager:debug("Unknown request when disconnected ~p", [Other]),
    {next_state, disconnected, State}.

connect_and_process(_Request, #state{} = State) -> %% todo: fix that the fist request is thrown away
    {ok, Socket} = loop_until_connected(State),
    {next_state, connected, State#state{socket=Socket}}.

loop_until_connected(State) ->
    case reconnect(State) of
        {ok, Socket} -> {ok, Socket};
        {error, Error} ->
            lager:warning("Cannot connect to socket ~p", [Error]),
            erlang:sleep(1000),
            loop_until_connected(State)
    end.

reconnect(#state{host=Host, port=Port} = _State) ->
    lager:info("Connecting  to ~p:~p", [Host, Port]),
    case gen_tcp:connect(Host, Port, [binary]) of
        {ok, Socket} ->
            lager:info("Connected socket to packet inspector"),
            put(sock_state, connected),
            {ok, Socket};
        {error, Reason} ->
            put(sock_state, {error, Reason}),
            lager:warning("Cannot connect to packet inspector ~p", [Reason]),
            {error, Reason}
    end.

-spec handle_info(Info::term(), State_Name::atom(), State::term()) ->
    {next_state, State_Name::atom(), NewState::term()} |
    {stop, Reason::term(), NewState::term()}.
handle_info(Msg, _State_Name, State) ->
    lager:info("Shutting down on unrequested handle_info ~p", [Msg]),
    {stop, shutdown, State}.

-spec handle_event(Msg::term(), StateName::atom(), StateData::term()) ->
    {next_state, NewStateName::atom(), NewStateData::term()} |
    {next_state, NewStateName::atom(), NewStateData::term(), Timeout::integer()} |
    {stop, Reason::atom(), Reply::term(), NewStateData::term()} |
    {stop, Reason::atom(), NewStateData::term()}.
handle_event(Msg, _State_Name, State_Data) ->
    lager:info("Shutting down on unrequested handle_event ~p", [Msg]),
    {stop, shutdown, State_Data}.

-spec handle_sync_event(Msg::term(), From::pid(), StateName::atom(), StateData::term()) ->
    {next_state, NewStateName::atom(), NewStateData::term()} |
    {reply, Reply::term(), NewStateName::atom(), NewStateData::term()} |
    {stop, Reason::atom(), NewStateData::term()}.
handle_sync_event(Msg, _From, _StateName, StateData) ->
    lager:info("Shutting down on unrequested handle_sync_event ~p", [Msg]),
    {stop, shutdown, StateData}.

-spec terminate(Reason::(normal | shutdown | {shutdown, term()} | term()), State_Name::atom(), State::term()) -> ok | term().
terminate(_Reason, _State_Name, _State) -> ok. 

-spec code_change(OldVsn::(term() | {down, term()}), State_Name::atom(), State::term(), Extra::term()) ->
    {ok, State_Name::atom(), NewState::term()} | {error, State_Name::atom(), Reason::term()}.
code_change(_OldVersion, State_Name, State, _Extra) -> {ok, State_Name, State}.

proplist_to_json(Proplist) ->
    [{[{<<"name">>, as_binary(Key)}, {<<"value">>, as_binary(Val)}]} || {Key,Val} <- Proplist].

as_binary(Val) when is_binary(Val) -> Val;
as_binary(Val) when is_list(Val) -> list_to_binary(Val);
as_binary(Val) when is_atom(Val) -> list_to_binary(atom_to_list(Val)).

-spec now_for_timestamp() -> string().
now_for_timestamp() ->
    {MegaSecs,Secs,MicroSecs} = now(),
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

get_account_data(undefined, _State) -> [];
get_account_data(Account, #state{account_callback_mod=Mod, account_callback_fun=Fun} = _State) ->
    case get(Account) of
        Account_Data when Account_Data =/= undefined -> Account_Data;
        undefined -> 
            case Mod:Fun(Account) of %% make callback here
                {Display_Name, Avatar} ->
                    Account_Data = [{<<"token">>, as_binary(Account)},
                        {<<"name">>, as_binary(Display_Name)},
                        {<<"avatar">>, as_binary(Avatar)}],
                    put(Account, Account_Data),
                    Account_Data;
                undefined -> []
            end
    end.

standard_pi_account_packet(Type, Account, Data, State) ->
    Account_Data = get_account_data(Account, State),
    jiffy:encode({[
                {<<"pi">>, {[
                            {<<"meta">>, {[
                                        {<<"request">>, Type},
                                        {<<"timestamp">>, now_for_timestamp()},
                                        {<<"account">>, {Account_Data}}
                                ]}},
                            {<<"data">>, {Data}}
                        ]}
                }]}).

is_tracing_on() ->
    ?IS_TRACING_ON.
