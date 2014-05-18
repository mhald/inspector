-module(pi_packets_sse).
-author('mhald@mac.com').

-behaviour(cowboy_http_handler).

-define(HTTP_OK, 200).
-define(HTTP_NO_RESPONSE, 204).
-define(HTTP_NOT_FOUND, 404).
-define(HTTP_METHOD_NOT_ALLOWED, 405).

-define(CHECK_TIMEOUT, 5000).
-define(MAX_RECENT_TRAFFIC_RECORDS, 100).

-record(state, {account_token          :: string(),
                resource               :: string(),
                handler_pid            :: pid()}).

-export([init/3,
         info/3,
         handle/2,
         terminate/2]).

-include("inspector.hrl").

-spec terminate(tuple(), #state{}) -> ok.
terminate(_Req, #state{account_token=Account_Token} = _State) ->
    lager:info("removing registered listener"),
    Account_Channel = pubsub:account_channel(Account_Token),
    pubsub:unsubscribe(Account_Channel, self()),
    ok.

-spec init({atom(), http}, Req, _) -> {loop | shutdown, Req, undefined | #state{}}.
init({_Any, http}, Req, _Opts) ->
    case cowboy_http_req:method(Req) of
        {'GET', Req0} ->
            {Method, Req1} = cowboy_http_req:method(Req0),
            {Account_Token, Req2} = cowboy_http_req:binding(account, Req1),
            case {Method, Account_Token} of
                 {'GET', undefined} ->
                     {ok, Req3} = cowboy_http_req:reply(?HTTP_NOT_FOUND, [], <<>>, Req2),
                     {shutdown, Req3, undefined};
                 {'GET', Account_Token} ->
                     Headers = [{'Content-Type', <<"text/event-stream">>}, {<<"Access-Control-Allow-Origin">>, <<"*">>}],
                     State =  #state{account_token = binary_to_list(Account_Token), handler_pid=self()},
                     {ok, Req3} = cowboy_http_req:chunked_reply(?HTTP_OK, Headers, Req2),
                     Account_Channel = pubsub:account_channel(Account_Token),
                     send_recent_traffic(Req3, Account_Token),
                     pubsub:safe_create_channel(Account_Channel),
                     pubsub:subscribe(Account_Channel, self()),
                     proc_lib:spawn_link(fun() -> health_check(Req, State) end),
                     {loop, Req3, State, infinity};
                 {Method, _} ->
                     lager:info("Wrong Method: ~p", [Method]),
                     {ok, Req3} = cowboy_http_req:reply(?HTTP_METHOD_NOT_ALLOWED, [], <<>>, Req2),
                     {shutdown, Req3, undefined}
            end;
        {_, Req0} ->
            {ok, Req1} = cowboy_http_req:reply(?HTTP_METHOD_NOT_ALLOWED, [], <<>>, Req0),
            {shutdown, Req1, undefined}
    end.

send_recent_traffic(Req, Current_User) ->
    [begin
                cowboy_http_req:chunk(["event: event-history\n"], Req),
                cowboy_http_req:chunk(["data:", Data, "\n\n"], Req)
        end || #history{data=Data, account=#account{token=Account_Token}} <- ets_buffer:history(history, ?MAX_RECENT_TRAFFIC_RECORDS), Account_Token =:= Current_User].

-spec handle(Req, #state{}) -> {shutdown, Req, #state{}}.
handle(Req, State) -> {shutdown, Req, State}.

info({'user login', Bin}, Req, State) ->
    cowboy_http_req:chunk(["event: user-login\n"], Req),
    cowboy_http_req:chunk(["data:", Bin, "\n\n"], Req),
    {loop, Req, State, hibernate};
info({'user logout', Bin}, Req, State) ->
    cowboy_http_req:chunk(["event: user-logout\n"], Req),
    cowboy_http_req:chunk(["data:", Bin, "\n\n"], Req),
    {loop, Req, State, hibernate};
info({'live event', Bin}, Req, State) ->
    cowboy_http_req:chunk(["event: live-event\n"], Req),
    cowboy_http_req:chunk(["data:", Bin, "\n\n"], Req),
    {loop, Req, State, hibernate};
info({cowboy_http_req, resp_sent}, Req, State) ->
    {loop, Req, State, hibernate};
info(client_disconnected, Req, State) ->
    lager:info("disconnected, shutting down"),
    {ok, Req, State};
info(Info, Req, State) ->
    lager:warning("Unexpected info: ~p", [Info]),
    {loop, Req, State, hibernate}.

health_check(Req, #state{handler_pid=Handler_Pid} = State) ->
    process_flag(trap_exit, true),
    {ok, Transport, Socket} = cowboy_http_req:transport(Req),
    receive
        {'EXIT', Handler_Pid, Reason} ->
            lager:info("~p died (~p). terminate/2 should've been called", [Handler_Pid, Reason]);
        Info ->
            lager:debug("~p received on ~p's health checker. Ignoring", [Info, Handler_Pid]),
            health_check(Req, State)
    after ?CHECK_TIMEOUT ->
        case Transport:recv(Socket, 0, 0) of
            {error, timeout} -> %% Still connected
                health_check(Req, State);
            {error, closed} ->
                Handler_Pid ! client_disconnected;
            {error, ebadf} ->
                Handler_Pid ! client_disconnected;
            SomethingElse ->
                lager:debug("~p still connected (maybe): ~p", [Socket, SomethingElse]),
                health_check(Req, State)
        end
    end.

