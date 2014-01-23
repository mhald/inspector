-module(pi_util).

-export([standard_pi_general_packet/2]).

-spec standard_pi_general_packet(binary(), any()) -> any().
standard_pi_general_packet(Type, Data) ->
    jiffy:encode({[
                {<<"pi">>, {[
                            {<<"meta">>, {[
                                        {<<"request">>, Type},
                                        {<<"timestamp">>, list_to_binary(pi_clock:timestamp())}
                                ]}},
                            {<<"data">>, {Data}}
                        ]}
                }]}).
