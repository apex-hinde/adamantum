-module(gameplay_changes).

init() ->
    ets:new(gameplay_updates, [ordered_set, private]).

