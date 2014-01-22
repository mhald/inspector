-module(pi_connection).

-behavior(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         start_link/0]).

-record(state, {
        socket :: pid(),
        server :: atom(),
        channels = [] :: list()
    }).

-record(request_metadata, {
        type :: atom(),
        account :: binary(),
        timestamp :: non_neg_integer()
    }).

-include("inspector.hrl").

-spec start_link() -> ok.
start_link() -> gen_server:start_link(?MODULE, [], []).

-spec init(_) -> {ok, #state{}}.
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

-spec handle_call(Request::term(), From::{pid(), Tag::term()},
    State::term()) ->
    {reply, Reply::term(), NewState::term()} |
    {reply, Reply::term(), NewState::term(), timeout() | hibernate} |
    {noreply, NewState::term()} |
    {noreply, NewState::term(), timeout() | hibernate} |
    {stop, Reason::term(), Reply::term(), NewState::term()}.
handle_call({set_socket, Socket}, _Caller, State) ->
    ok = inet:setopts(Socket, [{active, true}, {packet, line}, binary, {keepalive, true}, {nodelay, true}, {buffer, 1048576}, {recbuf, 1048576}]),
    {reply, ok, State#state{socket=Socket}};
handle_call(Request, Caller, State) ->
    lager:warning("Unhandled call ~p from ~p", [Request, Caller]),
    {noreply, State}.

-spec handle_info(Info::term(), State::term()) ->
    {noreply, NewState::term()} |
    {noreply, NewState::term(), timeout() | hibernate} |
    {stop, Reason::term(), NewState::term()}.
handle_info({tcp, _Sock, Data_Line}, #state{channels=Channels} = State) ->
    Data = binary:part(Data_Line, {0, byte_size(Data_Line)-1}),
    Json = jiffy:decode(Data),
    {ok, Channels2, Server} = 
    case parse_packet(Json) of
        {#request_metadata{} = Req_Metadata, Payload} ->
            handle_json(Req_Metadata, Payload, Data, Channels);
        _ ->
            lager:warning("Unhandled request packet ~p", [Json]),
            {ok, Channels}
    end,
    Next_State = case Server of 
        undefined -> State#state{channels=Channels2};
        _ -> State#state{channels=Channels2, server=Server}
    end,
    {noreply, Next_State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, shutdown, State};
handle_info(Msg, State) ->
    lager:info("Unhandled info ~p", [Msg]),
    {noreply, State}.

handle_json(#request_metadata{type=add_sessions}, Payload, Json, Channels) ->
    {Server, Account_Records} = parse_session_packet_data(Payload),
    pi_sessions:add(Server, Account_Records),
    pubsub:publish('user login', Json),
    {ok, Channels, Server};
handle_json(#request_metadata{type=remove_sessions}, Payload, Json, Channels) ->
    {Server, Account_Records} = parse_session_packet_data(Payload),
    pi_sessions:add(Server, Account_Records),
    pubsub:publish('user logout', Json),
    {ok, Channels, Server};
handle_json(#request_metadata{account=undefined}, Payload, _Json, Channels) ->
    lager:warning("Error, no account to broadcast on payload:~p", [Payload]),
    {ok, Channels, undefined};
handle_json(#request_metadata{type=Req_Type, timestamp=Timestamp,
        account=#account{token=Account_Token}=Account}, _Payload, Json, Channels)
        when Req_Type =:= data_packet;
             Req_Type =:= http_request; Req_Type =:= http_response ->
    lager:info("Registering send/recv request into history table for acct ~p", [Account]),
    ets_buffer:write(history, #history{account=Account, time=Timestamp, data=Json}),
    pubsub:publish('live event', Json),
    Channel = pubsub:account_channel(Account_Token),
    Next_Channels = case proplists:is_defined(Channel, Channels) of
        true ->
            pubsub:publish(Channel, Json),
            Channels;
        _ ->
            pubsub:safe_create_channel(Channel),
            pubsub:publish(Channel, Json),
            [{Channel, true} | Channels]
    end,
    {ok, Next_Channels, undefined};
handle_json(#request_metadata{type=Other_Req_Type}, Payload, Json, Channels) ->
    lager:warning("Unknown JSON packet type ~p/~p: ~p", [Other_Req_Type, Payload, Json]),
    {ok, Channels, undefined}.

-spec terminate(Reason::(normal | shutdown | {shutdown, term()} | term()), State::term()) -> ok | term().
terminate(_Reason, _State) ->
    lager:info("Closing socket listener"),
    ok.

-spec code_change(OldVsn::(term() | {down, term()}), State::term(), Extra::term()) ->
    {ok, NewState::term()} | {error, Reason::term()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

-spec handle_cast(Request::term(), State::term()) ->
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), timeout() | hibernate} |
        {stop, Reason :: term(), NewState :: term()}.
handle_cast(Request, State) ->
    lager:warning("Unhandled request ~p", [Request]),
    {noreply, State}.

-type packet_type() :: atom().
-type packet_timestamp() :: non_neg_integer().
-spec parse_packet(term()) -> undefined | {packet_type(), packet_timestamp()}.
parse_packet({Json}) ->
    case proplists:get_value(<<"pi">>, Json) of
        {Packet} ->
            case proplists:get_value(<<"meta">>, Packet) of
                {Metadata} when is_list(Metadata) -> 
                    Type = proplists:get_value(<<"request">>, Metadata),
                    Timestamp = proplists:get_value(<<"timestamp">>, Metadata),
                    Account = case proplists:get_value(<<"account">>, Metadata) of
                        {Account_Json} ->
                            parse_account(Account_Json);
                        _ ->
                            undefined
                    end,
                    Payload = case proplists:get_value(<<"data">>, Packet) of
                        {Payload_Data} -> Payload_Data;
                        _ -> undefined
                    end,
                    {#request_metadata{type=list_to_atom(binary_to_list(Type)), timestamp=Timestamp, account=Account}, Payload};
                undefined -> undefined
            end;
        undefined -> undefined
    end;
parse_packet(_) -> undefined.

parse_account(Account_Json) -> 
    #account{
        avatar=proplists:get_value(<<"avatar">>, Account_Json),
        token=proplists:get_value(<<"token">>, Account_Json),
        name=proplists:get_value(<<"name">>, Account_Json)
    }.

parse_session_packet_data(Payload) ->
    Server = proplists:get_value(<<"server">>, Payload),
    Server_Atom = list_to_atom(binary_to_list(Server)),
    Accounts = proplists:get_value(<<"accounts">>, Payload),
    Account_Records = [parse_account(Account) || {Account} <- Accounts],    
    {Server_Atom, Account_Records}.
