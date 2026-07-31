-module(chunk).
-export([all_stone/0, all_air/0, get_block_at_index/4, set_block_at_index/5, chunk_section_to_binary/1, paletted_container_to_binary/1, pack_data/2, air_chunk_column/0, get_chunk_from_chunk_column/2, insert_chunk_into_chunk_column/3, normal_chunk_column/2, encode_chunk_column_data/1, default_light_data/0, default_heightmaps_nbt/0, chunk_column_to_level_chunk_record/1, chunk_column_to_level_chunk_tuple/1]).
-include("chunk_records.hrl").
-include("records.hrl").



%% Types


get_chunk_from_chunk_column(ChunkColumn, Y) ->
    Index = Y bsr 4,
    maps:get(Index, ChunkColumn#chunk_column.chunk_sections).

insert_chunk_into_chunk_column(ChunkColumn, Y, ChunkSection) ->
    Index = Y bsr 4,
    NewSections = maps:put(Index, ChunkSection, ChunkColumn#chunk_column.chunk_sections),
    ChunkColumn#chunk_column{chunk_sections = NewSections}.    


get_block_at_index(Section, X, Y, Z) when X >= 0, X =< 15, Y >= 0, Y =< 15, Z >= 0, Z =< 15 ->
    Index = X + (Z bsl 4) + (Y bsl 8),
    BlockStates = Section#chunk_section.block_states,
    case BlockStates#paletted_container.type of 
        single_valued ->
            BlockStates#paletted_container.palette_index_to_id;
        indirect -> 
            Val = array:get(Index, BlockStates#paletted_container.data),
            maps:get(Val, BlockStates#paletted_container.palette_index_to_id);
        direct -> 
            array:get(Index, BlockStates#paletted_container.data)
    end.


set_block_at_index(X, Y, Z, BlockId, Section) ->
    Index = X + (Z bsl 4) + (Y bsl 8),
    Palette = Section#chunk_section.block_states,
    case Palette#paletted_container.type of 
        single_valued ->
            if 
                BlockId =/= Palette#paletted_container.palette_index_to_id ->
                    NewPalette2 = new_palette_map(Palette, 0, Palette#paletted_container.palette_index_to_id),
                    NewPalette3 = insert_into_palette_map(NewPalette2, Palette#paletted_container.top_index + 1, BlockId),
                    NewPalette4 = NewPalette3#paletted_container{type = indirect, data = array:set(Index, 1, Palette#paletted_container.data)},
                    Section#chunk_section{block_states = NewPalette4};
                true ->
                    Section
            end;
        indirect -> 
            case maps:is_key(BlockId, Palette#paletted_container.palette_id_to_index) of
                true ->
                    Val = maps:get(BlockId, Palette#paletted_container.palette_id_to_index),
                    Palette2 = Palette#paletted_container{data = array:set(Index, Val, Palette#paletted_container.data)},
                    Section#chunk_section{block_states = Palette2};
                false ->
                    TopIndex = Palette#paletted_container.top_index,
                    NewPalette2 = insert_into_palette_map(Palette, TopIndex + 1, BlockId),
                    NewPalette3 = NewPalette2#paletted_container{data = array:set(Index, TopIndex + 1 , Palette#paletted_container.data)},
                    Section#chunk_section{block_states = NewPalette3}
            end;
        direct -> 
            Palette2 = Palette#paletted_container{data = array:set(Index, BlockId, Palette#paletted_container.data)},
            Section#chunk_section{block_states = Palette2};
        _ ->
            Section
    end.



new_palette_map(Palette, LocalIndex, BlockId) ->
    Palette#paletted_container{
        palette_index_to_id = maps:from_list([{LocalIndex, BlockId}]),
        palette_id_to_index = maps:from_list([{BlockId, LocalIndex}]),
        top_index = LocalIndex
    }.

insert_into_palette_map(#paletted_container{palette_index_to_id = PaletteIdxToId, palette_id_to_index = PaletteIdToIdx} = Palette, LocalIndex, BlockId) ->
    Palette#paletted_container{
        palette_index_to_id = maps:put(LocalIndex, BlockId, PaletteIdxToId),
        palette_id_to_index = maps:put(BlockId, LocalIndex, PaletteIdToIdx),
        top_index = LocalIndex
    }.

chunk_section_to_binary(#chunk_section{non_air_blocks = NonAirBlocks, fluid_count = FluidCount, block_states = BlockStates, biomes = Biomes}) ->
    NonAirBlocksEncoded = encode:encode_type(NonAirBlocks, short),
    FluidCountEncoded = encode:encode_type(FluidCount, short),
    BlockStatesEncoded = paletted_container_to_binary(BlockStates),
    BiomesEncoded = paletted_container_to_binary(Biomes),

    <<NonAirBlocksEncoded/binary, FluidCountEncoded/binary, BlockStatesEncoded/binary, BiomesEncoded/binary>>.
    
paletted_container_to_binary(#paletted_container{bits_per_entry = BitsPerEntry, type = Type, palette_index_to_id = PaletteIndexToId, palette_id_to_index = _PaletteIdToIdx, top_index = TopIndex, data = Data}) ->
    BitsPerEntryEncoded = encode:encode_type(BitsPerEntry, ubyte),
    case Type of
        single_valued ->
            Val = if is_map(PaletteIndexToId) -> maps:get(0, PaletteIndexToId, 0); true -> PaletteIndexToId end,
            PaletteValEncoded = encode:encode_type(Val, varint),
            <<BitsPerEntryEncoded/binary, PaletteValEncoded/binary>>;
        indirect ->
            BlockIdsInOrder = [maps:get(I, PaletteIndexToId) || I <- lists:seq(0, TopIndex)],
            BlockIdsEncoded = encode:encode_type(BlockIdsInOrder, {prefixed_array, varint}),
            PackedLongs = pack_data(Data, BitsPerEntry),
            DataEncoded = list_to_binary([<<L:64/big-signed-integer>> || L <- PackedLongs]),
            <<BitsPerEntryEncoded/binary, BlockIdsEncoded/binary, DataEncoded/binary>>;
        direct ->
            PackedLongs = pack_data(Data, BitsPerEntry),
            DataEncoded = list_to_binary([<<L:64/big-signed-integer>> || L <- PackedLongs]),
            <<BitsPerEntryEncoded/binary, DataEncoded/binary>>
    end.

pack_data(Data, BitsPerEntry) when is_list(Data) ->
    EntriesPerLong = 64 div BitsPerEntry,
    pack_data_loop(Data, BitsPerEntry, EntriesPerLong, []);
pack_data(DataArray, BitsPerEntry) ->
    pack_data(array:to_list(DataArray), BitsPerEntry).

pack_data_loop([], _BitsPerEntry, _EntriesPerLong, Acc) ->
    lists:reverse(Acc);
pack_data_loop(RemainingData, BitsPerEntry, EntriesPerLong, Acc) ->
    {Chunk, Rest} = split_at(EntriesPerLong, RemainingData, []),
    Long = pack_long(Chunk, BitsPerEntry, 0, 0),
    pack_data_loop(Rest, BitsPerEntry, EntriesPerLong, [Long | Acc]).

split_at(0, List, Acc) ->
    {lists:reverse(Acc), List};
split_at(_N, [], Acc) ->
    {lists:reverse(Acc), []};
split_at(N, [H | T], Acc) ->
    split_at(N - 1, T, [H | Acc]).

pack_long([], _BitsPerEntry, _Shift, AccLong) ->
    AccLong;
pack_long([Val | Rest], BitsPerEntry, Shift, AccLong) ->
    MaskedVal = Val band ((1 bsl BitsPerEntry) - 1),
    NewLong = AccLong bor (MaskedVal bsl Shift),
    pack_long(Rest, BitsPerEntry, Shift + BitsPerEntry, NewLong).

encode_chunk_column_data(#chunk_column{chunk_sections = Sections}) ->
    %% Iterate in order from section -4 to 19
    SectionsBinary = list_to_binary([
        chunk_section_to_binary(maps:get(Idx, Sections)) 
        || Idx <- lists:seq(-4, 19)
    ]),
    DataLenEncoded = encode:encode_type(byte_size(SectionsBinary), varint),
    <<DataLenEncoded/binary, SectionsBinary/binary>>.



default_light_data() ->
    FullSkyLightArray = <<128, 16, (binary:copy(<<16#FF>>, 2048))/binary>>,
    #light_data{
        sky_light_mask = 16#3FFFFFF,  %% 26 bits set for 26 sections (-5 to 20)
        block_light_mask = 0,
        empty_sky_light_mask = 0,
        empty_block_light_mask = 16#3FFFFFF,  %% 26 bits set
        sky_light_arrays = lists:duplicate(26, FullSkyLightArray),
        block_light_arrays = []
    }.

default_heightmaps_nbt() ->
    #nbt{nbt = [{tag_compound, "", []}]}.

chunk_column_to_level_chunk_record({ok, ChunkColumn}) ->
    chunk_column_to_level_chunk_record(ChunkColumn);
chunk_column_to_level_chunk_record(#chunk_column{x = X, z = Z} = ChunkColumn) ->
    DataBinary = encode_chunk_column_data(ChunkColumn),
    #'minecraft:level_chunk_with_light'{
        chunk_x = X,
        chunk_z = Z,
        heightmaps = [],
        data = DataBinary,
        block_entities = [],
        light = default_light_data()
    }.

chunk_column_to_level_chunk_tuple({ok, ChunkColumn}) ->
    chunk_column_to_level_chunk_tuple(ChunkColumn);
chunk_column_to_level_chunk_tuple(#chunk_column{x = X, z = Z} = ChunkColumn) ->
    DataBinary = encode_chunk_column_data(ChunkColumn),
    {'minecraft:level_chunk_with_light', [
        X,
        Z,
        [],
        DataBinary,
        [],
        default_light_data()
    ]}.



%% initial setup

all_stone() ->
    Blocks = array:new(4096, [{default, 0}]),
    Biomes = array:new(64, [{default, 0}]),
    #chunk_section{
        non_air_blocks = 4096,
        fluid_count = 0,
        block_states = #paletted_container{
            bits_per_entry = 0,
            type = single_valued,
            palette_index_to_id = 1,
            palette_id_to_index = 1,
            top_index = 0,
            data = Blocks
        },
        biomes = #paletted_container{
            bits_per_entry = 0,
            type = single_valued,
            palette_index_to_id = 0,
            palette_id_to_index = 0,
            top_index = 0,
            data = Biomes
        }
    }.
all_air() ->
    Blocks = array:new(4096, [{default, 0}]),
    Biomes = array:new(64, [{default, 0}]),
    #chunk_section{
        non_air_blocks = 0,
        fluid_count = 0,
        block_states = #paletted_container{
            bits_per_entry = 0,
            type = single_valued,
            palette_index_to_id = 0,
            palette_id_to_index = 0,
            top_index = 0,
            data = Blocks
        },
        biomes = #paletted_container{
            bits_per_entry = 0,
            type = single_valued,
            palette_index_to_id = 0,
            palette_id_to_index = 0,
            top_index = 0,
            data = Biomes
        }
    }.

air_chunk_column() ->
    #chunk_column{
        x = 0,
        z = 0,
        min_y = -64,
        max_y = 320,
        chunk_sections = maps:from_list([{I, all_air()} || I <- lists:seq(-4, 19)])
    }.

normal_chunk_column(X, Z) ->
    #chunk_column{
        x = X,
        z = Z,
        min_y = -64,
        max_y = 320,
        chunk_sections = maps:from_list([{I, all_stone()} || I <- lists:seq(-4, 3)] ++ [{I, all_air()} || I <- lists:seq(4, 19)])
    }.



