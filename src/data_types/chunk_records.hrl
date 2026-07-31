-type bpe() :: non_neg_integer().
-type palette() :: 
      {single_valued, GlobalId :: non_neg_integer()}
    | {indirect, map}
    | direct.
%% Paletted Container Record
-record(paletted_container, {
    bits_per_entry :: bpe(),
    type :: single_valued | indirect | direct,
    palette_index_to_id :: palette(),
    palette_id_to_index :: palette(),
    top_index :: non_neg_integer(),
    data :: binary() | array:array(non_neg_integer()) | tuple()
}).
%% Chunk Section Record
-record(chunk_section, {
    non_air_blocks  = 0 :: integer(),                % 16-bit short
    fluid_count  = 0 :: integer(),                % 16-bit short
    block_states     :: #paletted_container{},   % 4,096 entries (16x16x16)
    biomes           :: #paletted_container{}    % 64 entries (4x4x4)
}).

-record(chunk_column,{
    x :: integer(),
    z :: integer(),
    min_y :: integer(),
    max_y :: integer(),
    chunk_sections :: maps:map(integer(), #chunk_section{})
    }).

-record('minecraft:level_chunk_with_light', {
    chunk_x,
    chunk_z,
    heightmaps,
    data,
    block_entities,
    light
}).

-record(block_entity, {
    packed_xz :: integer(),
    y :: integer(),
    type :: string(),
    nbt_data :: binary()
}).
