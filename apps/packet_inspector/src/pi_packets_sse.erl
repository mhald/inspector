-module(pi_packets_sse).
-author('mhald@mac.com').

-behaviour(cowboy_http_handler).

-define(HTTP_OK, 200).
-define(HTTP_NO_RESPONSE, 204).
-define(HTTP_NOT_FOUND, 404).
-define(HTTP_METHOD_NOT_ALLOWED, 405).

-define(CHECK_TIMEOUT, 5000).

-record(state, {account_token          :: string(),
                resource               :: string(),
                is_subscribed = false  :: boolean()}).

-export([init/3,
         handle/2,
         terminate/2]).

-spec init({atom(), http}, Req, _) -> {ok, Req, #state{}}.
init({_Any, http}, Req, _Opts) ->
    {ok, Req, #state{}}.

-spec handle(tuple(), #state{}) -> _.
handle(Req, State) ->
    {Method,        Req2} = cowboy_http_req:method(Req),
    {Account_Token, Req3} = cowboy_http_req:binding(account, Req2),
    case {Method, Account_Token} of
        {'GET', undefined} ->
            lager:info("Account token missing ~p", [Req]),
            {ok, Req4} = cowboy_http_req:reply(?HTTP_NOT_FOUND, [], <<>>, Req3),
            {ok, Req4, State};
        {'GET', Account_Token} ->
            Headers = [{'Content-Type', <<"text/event-stream">>}, {<<"Access-Control-Allow-Origin">>, <<"*">>}],
            {ok, Req4} = cowboy_http_req:chunked_reply(?HTTP_OK, Headers, Req3),
            start_loop(Req4, State#state{account_token = binary_to_list(Account_Token)});
        {Method, _} ->
            lager:info("Wrong Method: ~p", [Method]),
            {ok, Req4} = cowboy_http_req:reply(?HTTP_METHOD_NOT_ALLOWED, [], <<>>, Req3),
            {ok, Req4, State}
    end.

start_loop(Req, State) ->
    spawn_link(fun() -> health_check(Req, self()) end),
    handle_loop(Req, State).

subscribe(Account_Token, false) when is_list(Account_Token) ->
    Account_Channel = pubsub:account_channel(Account_Token),
    pubsub:subscribe(Account_Channel, self()),
    true;
subscribe(_Account_Token, true) ->
    true.

handle_loop(Req, #state{account_token=Account_Token, is_subscribed=Is_Subscribed} = State) ->
    Next_Is_Subscribed = subscribe(Account_Token, Is_Subscribed),
    NextState = State#state{is_subscribed=Next_Is_Subscribed},
    receive
        {cowboy_http_req, resp_sent} ->
            handle_loop(Req, NextState);
        {subscribed, _Channel, _Process} ->
            %eredis_sub:ack_message(Sub),
            handle_loop(Req, NextState);
        {broadcast, Bin} ->
            %eredis_sub:ack_message(Sub),
            cowboy_http_req:chunk(["data: ", Bin, "\n\n"], Req),
            handle_loop(Req, NextState);
        client_disconnected ->
            {ok, Req, NextState};
        Event ->
            lager:warning("Unhandled event to SSE packets: ~p", [Event]),
            handle_loop(Req, NextState)
    end.

health_check(Req, Handler_Pid) ->
    process_flag(trap_exit, true),
    {ok, Transport, Socket} = cowboy_http_req:transport(Req),
    receive
        {'EXIT', Handler_Pid, Reason} ->
            lager:info("~p died (~p). terminate/2 should've been called", [Handler_Pid, Reason]);
        Info ->
            lager:debug("~p received on ~p's health checker. Ignoring", [Info, Handler_Pid]),
            health_check(Req, Handler_Pid)
    after ?CHECK_TIMEOUT ->
        case Transport:recv(Socket, 0, 0) of
            {error, timeout} -> %% Still connected
                health_check(Req, Handler_Pid);
            {error, closed} ->
                Handler_Pid ! client_disconnected;
            {error, ebadf} ->
                Handler_Pid ! client_disconnected;
            SomethingElse ->
                lager:debug("~p still connected (maybe): ~p", [Socket, SomethingElse]),
                health_check(Req, Handler_Pid)
        end
    end.

-spec terminate(tuple(), #state{}) -> ok.
terminate(_Req, _State) -> ok.
