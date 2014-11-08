-module(pfuns_tests).
-include_lib("eunit/include/eunit.hrl").

-define(ONE_TO_FIVE, lists:seq(1,5)).
-define(NEGATIVE_LIST, lists:seq(-1,2)).
-define(BIG_LIST, lists:seq(1,100)).

pmap_empty_test() ->
    [] = pfuns:pmap(fun(El) -> El * 2 end, []).
pmap_test() ->
    [2,4,6,8,10] = pfuns:pmap(fun(El) -> El * 2 end, ?ONE_TO_FIVE).

pfilter_empty_test() ->
    [] = pfuns:pfilter(fun(El) -> El > 2 end, []).
pfiler_test() ->
    [3,4,5] = pfuns:pfilter(fun(El) -> El > 2 end, ?ONE_TO_FIVE).

pany_empty_test() ->
    false = pfuns:pany(fun(El) -> El > 2 end, []).
pany_true_test() ->
    true = pfuns:pany(fun(El) -> El > 2 end, ?ONE_TO_FIVE).
pany_false_test() ->
    false = pfuns:pany(fun(El) -> El > 2 end, ?NEGATIVE_LIST).

bmark_empty_test() -> [] = pfuns:bmark([]).
bmark_two_fun_test() ->
    [{_A,_B,_C}, {_X,_Y,_Z}] = pfuns:bmark([fun() ->
                              pfuns:pmap(fun(El) -> El * 2 end, ?BIG_LIST)
                      end,
                      fun() ->
                              lists:map(fun(El) -> El * 2 end, ?BIG_LIST)
                      end]).
