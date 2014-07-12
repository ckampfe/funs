-module(bench).
-export([bmark/1]).

%% provide a list of anonymous functions,
%% get back their run times

bmark(ListOfFns) ->
    bmark(ListOfFns, []).
bmark([], ListOfResults) ->
    lists:reverse(ListOfResults);
bmark(ListOfFns, ListOfResults) ->
    [F|RemainingFns] = ListOfFns,
    {H1,M1,S1} = time(),
    F(),
    {H2,M2,S2} = time(),
    bmark(RemainingFns, [{H2-H1, M2-M1, S2-S1} | ListOfResults]).
