-module(my_map).
-export([pmap/2]).

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
    gather(length(Pids), []).

gather(0, ResultsList) ->
    lists:reverse(ResultsList);
gather(RemainingCount, ResultsList) ->
    receive
        {ok, Result} -> gather(RemainingCount - 1, [Result|ResultsList])
    end.
