-module(adamantum_encode).
-export([login_success/3, set_compression/0,join_game/1, spawn_position/3, player_position_and_look/6, chunk_data/1,
        map_chunk_bulk/1]).
-include("records.hrl").

login_success(Data, UUID, Len) ->

    Len_of_name = varint:encode_varint(Len),
    Len_of_UUID = varint:encode_varint(byte_size(UUID)),
<<2,Len_of_UUID/binary, UUID/binary, Len_of_name/binary, Data/binary>>.

set_compression() ->
    Compression = varint:encode_varint(100000),
    <<3, -1>>.

join_game(Eid) -> 
    Gamemode = <<0>>,
    Dimension = <<0>>,
    Difficulty = <<2>>,
    Max_players = <<20>>,
    Level_type = <<"flat">>,
    Len_of_level_type = varint:encode_varint(byte_size(Level_type)),
    Reduced_debug_info = <<0>>,
    <<1, Eid/binary, Gamemode/binary, Dimension/binary, Difficulty/binary, Max_players/binary, Len_of_level_type/binary,
    Level_type/binary, Reduced_debug_info/binary>>.


spawn_position(X,Z,Y) ->
    <<5,X:24,Z:24,Y:16>>.


player_position_and_look(X, Z, Y, Yaw, Pitch, Relitive) ->
    <<8, X:64/float, Y:64/float, Z:64/float, Yaw:32/float, Pitch:32/float, Relitive>>.

    

chunk_data({X,Y}) ->
    Message = <<X:32,Y:32,1,1:3,0:13>>,
    Chunk = adamantum_chunk_manager:get_chunk_column({X,Y}),
    Chunks = Chunk#db_chunk_column.chunks,
    [Chunks_no_list|_T] = Chunks,
    Chunks_list = tuple_to_list(Chunks_no_list),
    Block_data = unpack_chunk_data(Chunks_list, <<>>),
    Biome_data = Chunk#db_chunk_column.biome,
    Chunk_data = <<Block_data/binary, Biome_data/binary>>,
    Length_of_data = varint:encode_varint(byte_size(Chunk_data)),
    Message2 = <<21, Message/binary, Length_of_data/binary, Chunk_data/binary>>,
    Packet_id = varint:encode_varint(33),
    <<Packet_id/binary,  Message2/binary>>.

unpack_chunk_data(Chunk, Acc) ->
    case (length(Chunk)) of
        0 ->
            Acc2 = Acc;
        _ ->
            [H|Tail] = Chunk,
            Chunk_data_1 = H#db_chunk.type,
            Chunk_data_2 = H#db_chunk.metadata,
            Chunk_data_3 = H#db_chunk.block_light,
            Chunk_data_4 = H#db_chunk.sky_light,
            Acc1 = <<Acc/binary, Chunk_data_1/binary, Chunk_data_2/binary, Chunk_data_3/binary, Chunk_data_4/binary>>,
            Acc2 = unpack_chunk_data(Tail, Acc1)
        end,
        Acc2.
        

map_chunk_bulk({_X,_Y}) ->
    Message = <<26,1,1,0,0,0:16>>,
    {Chunk,_Biome_data} = adamantum_chunk_manager:gen_column({0,0}),
    List_of_chunks = Chunk#db_chunk_column.chunks,
    <<Message/binary, List_of_chunks/binary>>.








