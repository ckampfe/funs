-module(pfuns).
-export([
         pmap/2, %% parallel map
         bmark/1, %% benchmark
         pfilter/2
        ]).


%% parallel map
%% results are indeterminably ordered
pmap(Function, List) ->
    Parent = self(),
    Pids = lists:map(
             fun(El) ->
                  spawn_link(fun() -> Parent ! {ok, catch(Function(El))} end)
             end,
             List
           ),
    gather_pmap(length(Pids), []).

gather_pmap(0, ResultsList) ->
    lists:reverse(ResultsList);
gather_pmap(RemainingCount, ResultsList) ->
    receive
        {ok, Result} -> gather_pmap(RemainingCount - 1, [Result|ResultsList])
    end.


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


%% parallel filter
%% results are indeterminably ordered
pfilter(Function, List) ->
    Parent = self(),
    Pids = lists:map(
             fun(El) ->
                  spawn_link(fun() ->
                       case Function(El) of
                           true -> Parent ! {ok, El};
                           _    -> Parent ! {false}
                       end
                  end)
             end,
             List
           ),
    gather_pfilter(length(Pids), []).

gather_pfilter(0, ResultsList) ->
    lists:reverse(ResultsList);
gather_pfilter(RemainingCount, ResultsList) ->
    receive
        {ok, Result} -> gather_pfilter(RemainingCount - 1, [Result|ResultsList]);
        {false}      -> gather_pfilter(RemainingCount - 1, ResultsList)
    end.
