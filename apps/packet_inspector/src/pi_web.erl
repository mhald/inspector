-module(pi_web).
-author('mhald@mac.com').

-export([start_link/0]).

%%
%% API Functions
%%
-define(DEFAULT_PORT, 9006).

-spec start_link() -> {ok,pid()}.
start_link() ->
  Port = config:get_env(http_port, ?DEFAULT_PORT),
  StaticOptions = [{directory, {priv_dir, packet_inspector, [<<"www">>]}},
      {etag, {attributes, [filepath, filesize, inode, mtime]}},
      {mimetypes, {fun mimetypes:path_to_mimes/2, default}}],
  Dispatch = [
      %% {Host, list({Path, Handler, Opts})}
      {'_', [
              {[<<"stream">>, <<"sessions">>], pi_sessions_sse, []},
              {[<<"stream">>, <<"packets">>, account], pi_packets_sse, []},
              {[<<"stream">>, <<"packets">>, account, resource], pi_packets_sse, []},
              {[<<"packets">>, '...'], cowboy_http_static, StaticOptions ++ [{file, <<"packets.html">>}]},
              {[], cowboy_http_static, StaticOptions ++ [{file, <<"index.html">>}]}, %% http request at root
              {['...'], cowboy_http_static, StaticOptions}
          ]
      }],

  lager:info("Starting packet inspector web on port ~p", [Port]),

  %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
  cowboy:start_listener(packet_inspector_listener, 100,
                        cowboy_tcp_transport, [{port, Port}],
                        cowboy_http_protocol, [{dispatch, Dispatch}]
                       ).
