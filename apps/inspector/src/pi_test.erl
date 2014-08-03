-module(pi_test).
-author('mhald@mac.com').

-export([simulate_client/0]).

-spec simulate_client() -> ok.
simulate_client() ->
    pi_client:send_packet("mhald", <<"sample_encoding">>, "test1"),
    pi_client:http_request("mhald", "/v1/test", [{<<"header1_name">>,<<"header1_value">>}], [{"param1_name","param1_value"}]),
    ok.
