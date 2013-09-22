-module(pi_sessions_sse).
-author('mhald@mac.com').

-behaviour(cowboy_http_handler).

-define(HTTP_OK, 200).
-define(HTTP_NO_RESPONSE, 204).
-define(HTTP_NOT_FOUND, 404).
-define(HTTP_METHOD_NOT_ALLOWED, 405).

-define(CHECK_TIMEOUT, 5000).

-record(state, {is_subscribed = false  :: boolean()}).

-export([init/3,
         handle/2,
         terminate/2]).

-include("inspector.hrl").

-spec init({atom(), http}, Req, _) -> {ok, Req, #state{}}.
init({_Any, http}, Req, _Opts) ->
    {ok, Req, #state{}}.

-spec handle(tuple(), #state{}) -> _.
handle(Req, State) ->
    {Method, Req2} = cowboy_http_req:method(Req),
    {undefined, Req3} = cowboy_http_req:binding(account, Req2),
    case {Method} of
        {'GET'} ->
            Headers = [{'Content-Type', <<"text/event-stream">>}, {<<"Access-Control-Allow-Origin">>, <<"*">>}],
            {ok, Req4} = cowboy_http_req:chunked_reply(?HTTP_OK, Headers, Req3),
            send_connected_users(Req4),
            send_recent_traffic(Req4),
            start_loop(Req4, State);
        {Method} ->
            lager:info("Wrong Method: ~p", [Method]),
            {ok, Req4} = cowboy_http_req:reply(?HTTP_METHOD_NOT_ALLOWED, [], <<>>, Req3),
            {ok, Req4, State}
    end.

start_loop(Req, State) ->
    spawn_link(fun() -> health_check(Req, self()) end),
    handle_loop(Req, State).

subscribe() ->
    pubsub:subscribe('live event', self()),
    pubsub:subscribe('user login', self()),
    pubsub:subscribe('user logout', self()).

send_connected_users(Req) ->
    Accounts = pi_sessions:get_sessions(),
    Accounts_Data = [{get_account_data(Account)} || Account <- Accounts],
    Text = pi_util:standard_pi_general_packet(<<"add_sessions">>, [
        {<<"server">>, list_to_binary(atom_to_list(node()))},
        {<<"accounts">>, Accounts_Data}
    ]),
lager:info("Sending connected users ~p", [Text]),
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

handle_loop(Req, #state{is_subscribed=Is_Subscribed} = State) ->
    Next_Is_Subscribed = case Is_Subscribed of
        false -> subscribe(), true;
        true -> true
    end,
    NextState = State#state{is_subscribed=Next_Is_Subscribed},
    receive
        {cowboy_http_req, resp_sent} ->
            handle_loop(Req, NextState);
        {subscribed, _Channel, _Process} ->
            handle_loop(Req, NextState);
        %%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Start pubsub handlers %%
        {'user login', Bin} ->
            cowboy_http_req:chunk(["event: login-user\n"], Req),
            cowboy_http_req:chunk(["data:", Bin, "\n\n"], Req),
            handle_loop(Req, NextState);
        {'user logout', Bin} ->
            cowboy_http_req:chunk(["event: logout-user\n"], Req),
            cowboy_http_req:chunk(["data:", Bin, "\n\n"], Req),
            handle_loop(Req, NextState);
        {'live event', Bin} ->
            cowboy_http_req:chunk(["event: live-event\n"], Req),
            cowboy_http_req:chunk(["data:", Bin, "\n\n"], Req),
            handle_loop(Req, NextState);
        %% End pubsub handlers %%
        %%%%%%%%%%%%%%%%%%%%%%%%%
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
