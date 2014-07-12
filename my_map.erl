-module(my_map).
-export([pmap/2]).

%% parallel #map
%% currently: one process per list element
%% this is not efficient, consider sharding
%% the list into chunks based on size,
%% with each process consuming a fraction of the total
%% list

pmap(Function, List) ->
    pmap(self(), Function, List),
    receive
        {ok, Results} -> lists:reverse(Results)
    end.
pmap(MainPid, Function, List) ->

    %% spawn results aggregator
    ResultsPid = spawn(fun() -> result_aggregator(MainPid, length(List), []) end),

    %% worker
    ItemWorker = fun(Item) ->
                     Result = Function(Item),
                     ResultsPid ! {ok, Result}
                 end,

    %% foreach
    [spawn(fun() -> ItemWorker(Item) end) || Item <- List],
    ok.


%% aggregate results
result_aggregator(MainPid, RemainingCount, ResultsList) ->
    receive
        {ok, Result} ->
            case RemainingCount of
                N when N =:= 1 ->
                    MainPid ! {ok, [Result|ResultsList]};
                _ ->
                    result_aggregator(MainPid, RemainingCount - 1, [Result|ResultsList])
            end
    end.
