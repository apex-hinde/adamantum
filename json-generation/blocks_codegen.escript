-module(blocks_codegen).
-export([main/1, generate/3]).

main([]) ->
    generate("json-26.2/blocks.json", "src/blocks/block_type_registry.erl", "src/blocks/block_properties_registry.erl");
main(_) ->
    io:format("Usage: escript blocks_codegen.escript~n"),
    halt(1).

generate(JsonPath, NamesOutErlPath, PropsOutErlPath) ->
    {ok, Bin} = file:read_file(JsonPath),
    BlocksMap = json:decode(Bin),

    %% Collect unique block names, state ID ranges -> name, default state ID, and state ID -> properties
    {NameAtomPairs, IdRangeToNameList, DefaultIdList, IdToPropsList} =
        maps:fold(
            fun(BlockNameBin, BlockData, {AccNames, AccRangeName, AccDefId, AccIdProps}) ->
                BlockNameAtom = binary_to_atom(BlockNameBin, utf8),
                States = maps:get(<<"states">>, BlockData, []),

                NewAccNames = [{BlockNameBin, BlockNameAtom} | AccNames],

                {StatesAcc, DefaultId, Ranges} = process_states(States),

                NewAccRangeName = [
                    {Min, format_range_clause(Range, BlockNameAtom)}
                 || Range = {Min, _Max} <- Ranges
                ] ++ AccRangeName,

                NewAccIdProps = [
                    {Id, io_lib:format("id_to_properties(~p) -> ~p;", [Id, Props])}
                 || {Id, Props} <- StatesAcc
                ] ++ AccIdProps,

                NewAccDefId = [
                    io_lib:format("default_state_id('~s') -> ~p;", [BlockNameAtom, DefaultId])
                    | AccDefId
                ],

                {NewAccNames, NewAccRangeName, NewAccDefId, NewAccIdProps}
            end,
            {[], [], [], []},
            BlocksMap
        ),

    %% --- 1. GENERATE TYPE / NAME REGISTRY ---
    SortedNames = lists:keysort(1, NameAtomPairs),
    NameToAtomClauses = [
        io_lib:format("name_to_atom(~p) -> '~s';", [NameBin, Atom])
     || {NameBin, Atom} <- SortedNames
    ],
    AtomToNameClauses = [
        io_lib:format("atom_to_name('~s') -> ~p;", [Atom, NameBin])
     || {NameBin, Atom} <- SortedNames
    ],
    SortedRangeToName = [Clause || {_Min, Clause} <- lists:keysort(1, IdRangeToNameList)],
    SortedDefaultId = lists:sort(DefaultIdList),

    NamesHeader = "-module(block_type_registry).\n"
                  "-export([name_to_atom/1, atom_to_name/1, id_to_name/1, default_state_id/1]).\n\n",

    CatchNameToAtom = "name_to_atom(_) -> error(unknown_name).\n\n",
    CatchAtomToName = "atom_to_name(_) -> error(unknown_atom).\n\n",
    CatchIdToName = "id_to_name(_) -> error(unknown_id).\n\n",
    CatchDefaultId = "default_state_id(_) -> error(unknown_block).\n",

    NamesContent = [
        NamesHeader,
        lists:join("\n", NameToAtomClauses), "\n", CatchNameToAtom,
        lists:join("\n", AtomToNameClauses), "\n", CatchAtomToName,
        lists:join("\n", SortedRangeToName), "\n", CatchIdToName,
        lists:join("\n", SortedDefaultId), "\n", CatchDefaultId
    ],
    file:write_file(NamesOutErlPath, unicode:characters_to_binary(NamesContent)),
    io:format("Generated ~s successfully.~n", [NamesOutErlPath]),

    %% --- 2. GENERATE PROPERTIES REGISTRY ---
    SortedIdToProps = [Clause || {_Id, Clause} <- lists:keysort(1, IdToPropsList)],

    PropsHeader = "-module(block_properties_registry).\n"
                  "-export([id_to_properties/1]).\n\n",

    CatchIdToProps = "id_to_properties(_) -> error(unknown_id).\n",

    PropsContent = [
        PropsHeader,
        lists:join("\n", SortedIdToProps), "\n", CatchIdToProps
    ],
    file:write_file(PropsOutErlPath, unicode:characters_to_binary(PropsContent)),
    io:format("Generated ~s successfully.~n", [PropsOutErlPath]).

process_states(States) ->
    {StatesAcc, DefaultId, Ids} = lists:foldl(
        fun(StateMap, {AccStates, AccDefaultId, AccIds}) ->
            Id = maps:get(<<"id">>, StateMap),
            Props = maps:get(<<"properties">>, StateMap, #{}),
            IsDefault = maps:get(<<"default">>, StateMap, false),
            NewDefaultId = case IsDefault of
                true -> Id;
                false -> AccDefaultId
            end,
            {[{Id, Props} | AccStates], NewDefaultId, [Id | AccIds]}
        end,
        {[], 0, []},
        States
    ),
    Ranges = group_ranges(lists:sort(Ids)),
    {StatesAcc, DefaultId, Ranges}.

group_ranges([]) -> [];
group_ranges([H | T]) ->
    group_ranges(T, H, H, []).

group_ranges([], Min, Max, Acc) ->
    lists:reverse([{Min, Max} | Acc]);
group_ranges([H | T], Min, Max, Acc) when H == Max + 1 ->
    group_ranges(T, Min, H, Acc);
group_ranges([H | T], Min, Max, Acc) ->
    group_ranges(T, H, H, [{Min, Max} | Acc]).

format_range_clause({Min, Min}, BlockNameAtom) ->
    io_lib:format("id_to_name(~p) -> '~s';", [Min, BlockNameAtom]);
format_range_clause({Min, Max}, BlockNameAtom) ->
    io_lib:format("id_to_name(Id) when Id >= ~p, Id =< ~p -> '~s';", [Min, Max, BlockNameAtom]).
