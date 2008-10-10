-module(uuid).
-export([gen/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% External Functions

gen() ->
    lists:concat([to_hex(crypto:rand_bytes(4)), '-',
                  to_hex(crypto:rand_bytes(2)), '-',
                  to_hex(crypto:rand_bytes(2)), '-',
                  to_hex(crypto:rand_bytes(2)), '-',
                  to_hex(crypto:rand_bytes(6))]).

%% Internal Functions

%% The following to_hex, to_digit functions are borrowed from CouchDB
%% and are Apache 2.0 License http://www.apache.org/licenses/LICENSE-2.0

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.

-ifdef(TEST).

unique_test_() ->
    {setup,
     fun() -> crypto:start() end,
     fun(_) -> crypto:stop() end,
     [fun test_uuid/0, fun test_uuid/0]}.

test_uuid() ->
    X=fixture(),
    Y=fixture(),
    ?assertError({badmatch,_}, X=Y).

fixture() -> X=gen(), X.

-endif.
