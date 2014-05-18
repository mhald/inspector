-module(pi_sessions_sse).
-author('mhald@mac.com').

-behaviour(cowboy_http_handler).

-define(HTTP_OK, 200).
-define(HTTP_NO_RESPONSE, 204).
-define(HTTP_NOT_FOUND, 404).
-define(HTTP_METHOD_NOT_ALLOWED, 405).
-define(CHECK_TIMEOUT, 5000).
-define(LIVE_EVENT, 'live event').
-define(USER_LOGIN, 'user login').
-define(USER_LOGOUT, 'user logout').

-record(state, {is_subscribed = false  :: boolean()}).

-export([init/3,
         info/3,
         handle/2,
         terminate/2]).

-include("inspector.hrl").

-spec init({atom(), http}, Req, _) -> {loop | shutdown, Req, undefined | #state{}}.
init({_Any, http}, Req, _Opts) ->
    case cowboy_http_req:method(Req) of
        {'GET', Req0} ->
            {Method, Req2} = cowboy_http_req:method(Req0),
            {undefined, Req3} = cowboy_http_req:binding(account, Req2),
            case {Method} of
                {'GET'} ->
                    State =  #state{},
                    Headers = [{'Content-Type', <<"text/event-stream">>}, {<<"Access-Control-Allow-Origin">>, <<"*">>}],
                    {ok, Req4} = cowboy_http_req:chunked_reply(?HTTP_OK, Headers, Req3),
                    send_connected_users(Req4),
                    send_recent_traffic(Req4),
                    Handler = self(),
                    proc_lib:spawn_link(fun() -> health_check(Req, Handler) end),
                    pubsub:subscribe(?LIVE_EVENT, self()),
                    pubsub:subscribe(?USER_LOGIN, self()),
                    pubsub:subscribe(?USER_LOGOUT, self()),
                    {{_Host,Port}, Req5} = cowboy_http_req:peer(Req4),
                    Proc_Name = list_to_atom(binary_to_list(iolist_to_binary(["homepage_sse_", integer_to_list(Port)]))),
                    erlang:register(Proc_Name, self()),
                    {loop, Req5, State, infinity};
                {Method} ->
                    lager:info("Wrong Method: ~p", [Method]),
                    {ok, Req4} = cowboy_http_req:reply(?HTTP_METHOD_NOT_ALLOWED, [], <<>>, Req3),
                    {shutdown, Req4, undefined}
            end;
        {_, Req0} ->
            {ok, Req1} = cowboy_http_req:reply(?HTTP_METHOD_NOT_ALLOWED, [], <<>>, Req0),
            {shutdown, Req1, undefined}
    end.

-spec handle(Req, #state{}) -> {shutdown, Req, #state{}}.
handle(Req, State) -> {shutdown, Req, State}.

send_connected_users(Req) ->
    Accounts = pi_sessions:get_sessions(),
    Accounts_Data = [{get_account_data(Account)} || Account <- Accounts],
    Text = pi_util:standard_pi_general_packet(<<"add_sessions">>, [
        {<<"server">>, list_to_binary(atom_to_list(node()))},
        {<<"accounts">>, Accounts_Data}
    ]),
    cowboy_http_req:chunk(["event: connected-users\n"], Req),
    cowboy_http_req:chunk(["data: ", Text, "\n\n"], Req).

get_account_data(#account{name=Display_Name, avatar=Avatar, token=Account_Token} = _Account) ->
    [{<<"token">>, Account_Token},
     {<<"name">>, Display_Name},
     {<<"avatar">>, Avatar}].

send_recent_traffic(Req) ->
    [begin 
                cowboy_http_req:chunk(["event: event-history\n"], Req),
                cowboy_http_req:chunk(["data:", Data, "\n\n"], Req) 
        end || #history{data=Data} <- ets_buffer:history(history, 10)].

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
    lager:info("sessions sse disconnected, shutting down"),
    {ok, Req, State};
info(Info, Req, State) ->
    lager:warning("Unexpected info: ~p", [Info]),
    {loop, Req, State, hibernate}.

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
terminate(_Req, _State) ->
    pubsub:unsubscribe(?LIVE_EVENT, self()),
    pubsub:unsubscribe(?USER_LOGIN, self()),
    pubsub:unsubscribe(?USER_LOGOUT, self()),
    ok.
