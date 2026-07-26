-module(packets_codegen).
-export([main/1, generate/3]).

main([]) ->
    ProtocolStates = [configuration, handshake, login, play, status],
    lists:foreach(fun(PS) -> 
        OutErlPath = lists:flatten(io_lib:format("src/packets/~s.erl", [atom_to_list(PS)])),
        generate("json-26.2/packets.json", OutErlPath, PS)
    end, ProtocolStates);
main(_) ->
    io:format("Usage: escript packets_codegen.escript~n"),
    halt(1).

generate(JsonPath, OutErlPath, ProtocolState) ->
    {ok, Bin} = file:read_file(JsonPath),
    Data = json:decode(Bin), 
    
    ProtocolStateBin = atom_to_binary(ProtocolState, utf8),
    StateMap = maps:get(ProtocolStateBin, Data, #{}),
    Clientbound = maps:get(<<"clientbound">>, StateMap, #{}),
    Serverbound = maps:get(<<"serverbound">>, StateMap, #{}),

    PairsClientbound = maps:fold(fun(NameBin, #{<<"protocol_id">> := Id}, Acc) ->
        [{Id, NameBin} | Acc]
    end, [], Clientbound),
    SortedPairsClientbound = lists:keysort(1, PairsClientbound),

    PairsServerbound = maps:fold(fun(NameBin, #{<<"protocol_id">> := Id}, Acc) ->
        [{Id, NameBin} | Acc]
    end, [], Serverbound),
    SortedPairsServerbound = lists:keysort(1, PairsServerbound),

    IdToNameClausesClient = [
        io_lib:format("id_to_name(clientbound, ~p) -> '~s';", [Id, NameBin])
        || {Id, NameBin} <- SortedPairsClientbound
    ],
    IdToNameClausesServer = [
        io_lib:format("id_to_name(serverbound, ~p) -> '~s';", [Id, NameBin])
        || {Id, NameBin} <- SortedPairsServerbound
    ],

    NameToIdClausesClient = [
        io_lib:format("name_to_id(clientbound, '~s') -> ~p;", [NameBin, Id])
        || {Id, NameBin} <- SortedPairsClientbound
    ],
    NameToIdClausesServer = [
        io_lib:format("name_to_id(serverbound, '~s') -> ~p;", [NameBin, Id])
        || {Id, NameBin} <- SortedPairsServerbound
    ],

    Header = io_lib:format("-module(~s).\n-export([id_to_name/2, name_to_id/2]).\n\n", [atom_to_list(ProtocolState)]),
    CatchIdToName = "id_to_name(_, _) -> error(unknown_id).\n\n",
    CatchNameToId = "name_to_id(_, _) -> error(unknown_name).\n",

    AllIdToNameClauses = IdToNameClausesClient ++ IdToNameClausesServer,
    AllNameToIdClauses = NameToIdClausesClient ++ NameToIdClausesServer,

    Content = [
        Header,
        case AllIdToNameClauses of
            [] -> "";
            _ -> [lists:join("\n", AllIdToNameClauses), "\n"]
        end,
        CatchIdToName,
        case AllNameToIdClauses of
            [] -> "";
            _ -> [lists:join("\n", AllNameToIdClauses), "\n"]
        end,
        CatchNameToId
    ],
    file:write_file(OutErlPath, unicode:characters_to_binary(Content)).