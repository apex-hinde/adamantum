-module(chunk_tests).
-include_lib("eunit/include/eunit.hrl").

all_air_test() ->
    Section = chunk:all_air(),
    ?assertEqual(0, chunk:get_block_at_index(Section, 0, 0, 0)),
    ?assertEqual(0, chunk:get_block_at_index(Section, 15, 15, 15)),
    ?assertEqual(0, chunk:get_block_at_index(Section, 5, 2, 8)).

all_stone_test() ->
    Section = chunk:all_stone(),
    ?assertEqual(1, chunk:get_block_at_index(Section, 0, 0, 0)),
    ?assertEqual(1, chunk:get_block_at_index(Section, 15, 15, 15)),
    ?assertEqual(1, chunk:get_block_at_index(Section, 5, 2, 8)).

set_block_single_to_indirect_test() ->
    Section = chunk:all_air(),
    %% Set block at (5, 2, 3) to Oak Log (ID 1520)
    NewSection = chunk:set_block_at_index(5, 2, 3, 1520, Section),
    
    %% The modified coordinate should be 1520
    ?assertEqual(1520, chunk:get_block_at_index(NewSection, 5, 2, 3)),
    
    %% Unmodified coordinates should remain 0 (Air)
    ?assertEqual(0, chunk:get_block_at_index(NewSection, 0, 0, 0)),
    ?assertEqual(0, chunk:get_block_at_index(NewSection, 5, 2, 4)).

set_block_existing_indirect_test() ->
    Section0 = chunk:all_air(),
    %% Add block 10 (Stone)
    Section1 = chunk:set_block_at_index(1, 1, 1, 10, Section0),
    %% Add block 1520 (Log)
    Section2 = chunk:set_block_at_index(2, 2, 2, 1520, Section1),
    %% Set another coordinate to 10 (Stone - existing in palette)
    Section3 = chunk:set_block_at_index(3, 3, 3, 10, Section2),

    ?assertEqual(10, chunk:get_block_at_index(Section3, 1, 1, 1)),
    ?assertEqual(1520, chunk:get_block_at_index(Section3, 2, 2, 2)),
    ?assertEqual(10, chunk:get_block_at_index(Section3, 3, 3, 3)),
    ?assertEqual(0, chunk:get_block_at_index(Section3, 0, 0, 0)).

pack_data_test() ->
    %% 16 entries of 4-bit values: 0..15
    Data = lists:seq(0, 15),
    Packed = chunk:pack_data(Data, 4),
    ?assertEqual(1, length(Packed)),
    [PackedLong] = Packed,
    ?assert(is_integer(PackedLong)),
    ?assert(PackedLong > 0).

chunk_section_to_binary_test() ->
    Section = chunk:all_air(),
    Binary = chunk:chunk_section_to_binary(Section),
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0).

air_chunk_column_test() ->
    Column = chunk:air_chunk_column(),
    SectionMin = chunk:get_chunk_from_chunk_column(Column, -64),
    ?assertEqual(0, chunk:get_block_at_index(SectionMin, 0, 0, 0)),
    SectionMax = chunk:get_chunk_from_chunk_column(Column, 319),
    ?assertEqual(0, chunk:get_block_at_index(SectionMax, 0, 0, 0)),
    %% Verify keys in chunk_sections map range from -4 to 19
    Sections = element(6, Column), % chunk_column.chunk_sections
    ExpectedKeys = lists:seq(-4, 19),
    ?assertEqual(ExpectedKeys, lists:sort(maps:keys(Sections))).

level_chunk_with_light_test() ->
    Column = chunk:normal_chunk_column(3, 7),
    Record = chunk:chunk_column_to_level_chunk_record(Column),
    ?assertEqual(3, element(2, Record)), %% chunk_x
    ?assertEqual(7, element(3, Record)), %% chunk_z
    ?assert(is_binary(element(5, Record))), %% data

    Tuple = chunk:chunk_column_to_level_chunk_tuple(Column),
    ?assertEqual('minecraft:level_chunk_with_light', element(1, Tuple)),
    {'minecraft:level_chunk_with_light', [X, Z, _Heightmaps, DataBin, _BlockEntities, _Light]} = Tuple,
    ?assertEqual(3, X),
    ?assertEqual(7, Z),
    ?assert(is_binary(DataBin)).

encode_level_chunk_with_light_packet_test() ->
    Column = chunk:normal_chunk_column(3, 7),
    Tuple = chunk:chunk_column_to_level_chunk_tuple(Column),
    PacketBinary = encode:encode_message(Tuple, 'minecraft:level_chunk_with_light', 5),
    ?assert(is_binary(PacketBinary)),
    ?assert(byte_size(PacketBinary) > 0).





