-module(registry_codegen).
-export([main/1, generate/3]).

main([NameSpace]) ->
    [_, NameSpace2] = string:split(NameSpace, ":"),
    OutErlPath = io_lib:format("src/registries/~s_type_registry.erl", [NameSpace2]),
    generate("json-26.2/registries.json", OutErlPath, NameSpace);
main(_) ->
    io:format("Usage: escript registry_codegen.escript [NameSpace]~n"),
    halt(1).

generate(JsonPath, OutErlPath, NameSpace) ->
    {ok, Bin} = file:read_file(JsonPath),
    Data = json:decode(Bin), 
    
    NameSpaceBin = list_to_binary(NameSpace),
    #{NameSpaceBin := #{<<"entries">> := Entries}} = Data,
    Pairs = maps:fold(fun(NameBin, #{<<"protocol_id">> := Id}, Acc) ->
        [{Id, NameBin} | Acc]
    end, [], Entries),
    SortedPairs = lists:keysort(1, Pairs),
    NameToIdClauses = [
        io_lib:format("name_to_id('~s') -> ~p;", [NameBin, Id])
        || {Id, NameBin} <- SortedPairs
    ],
    IdToNameClauses = [
        io_lib:format("id_to_name(~p) -> '~s';", [Id, NameBin])
        || {Id, NameBin} <- SortedPairs
    ],
    [_, NameSpace2] = string:split(NameSpace, ":"),
    Header = io_lib:format("-module(~s_type_registry).\n-export([name_to_id/1, id_to_name/1]).\n\n", [NameSpace2]),
    CatchNameToId = "name_to_id(_) -> error(unknown_name).\n\n",
    CatchIdToName = "id_to_name(_) -> error(unknown_id).\n",
    Content = [
        Header,
        lists:join("\n", NameToIdClauses), "\n", CatchNameToId,
        lists:join("\n", IdToNameClauses), "\n", CatchIdToName
    ],
    file:write_file(OutErlPath, unicode:characters_to_binary(Content)).
