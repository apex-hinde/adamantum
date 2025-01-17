-module(adamantum_encode).
-export([login_success/3, set_compression/0,join_game/1, spawn_position/3, player_position_and_look/6, chunk_data/1,
        map_chunk_bulk/1]).
-include("records.hrl").



login_success(Data, UUID, Len) ->

    Len_of_name = varint:encode_varint(Len),
    Len_of_UUID = varint:encode_varint(byte_size(UUID)),
    Message = <<02,Len_of_UUID/binary, UUID/binary, Len_of_name/binary, Data/binary>>,

    Length = varint:encode_varint(byte_size(Message)),

    <<Length/binary, Message/binary>>.

set_compression() ->
    Uncompressed_limit = varint:encode_varint(250),
    <<2,3,-1>>.

join_game(Eid) -> 
    Gamemode = <<0>>,
    Dimension = <<0>>,
    Difficulty = <<2>>,
    Max_players = <<20>>,
    Level_type = <<"flat">>,
    Len_of_level_type = varint:encode_varint(byte_size(Level_type)),
    Reduced_debug_info = <<0>>,
    Message = <<01, Eid/binary, Gamemode/binary, Dimension/binary, Difficulty/binary, Max_players/binary, Len_of_level_type/binary,
    Level_type/binary, Reduced_debug_info/binary>>,
    Length_of_message = varint:encode_varint(byte_size(Message)),
    <<Length_of_message/binary, Message/binary>>.

spawn_position(X,Z,Y) ->

    Message = <<5,X:24,Z:24,Y:16>>,
    Length_of_message = varint:encode_varint(byte_size(Message)),
    <<Length_of_message/binary, Message/binary>>.


player_position_and_look(X, Z, Y, Yaw, Pitch, Reletive) ->
   Message =  <<8, X:64/float, Y:64/float, Z:64/float, Yaw:32/float, Pitch:32/float, Reletive>>,
   Length_of_message = varint:encode_varint(byte_size(Message)),
   <<Length_of_message/binary, Message/binary>>.
    

chunk_data({X,Y}) ->

    Message = <<X:32,Y:32,1,1:3,0:13>>,
    Chunk = adamantum_chunk_manager:get_chunk_column({X,Y}),
    Chunks = Chunk#db_chunk_column.chunks,
    [Chunks_no_list|T] = Chunks,
    Chunks_list = tuple_to_list(Chunks_no_list),
    Block_data = unpack_chunk_data(Chunks_list, <<>>),
    Biome_data = Chunk#db_chunk_column.biome,
    Chunk_data = <<Block_data/binary, Biome_data/binary>>,
    Length_of_data = varint:encode_varint(byte_size(Chunk_data)),
    Message2 = <<21, Message/binary, Length_of_data/binary, Chunk_data/binary>>,
    Length_of_Message = varint:encode_varint(byte_size(Message2)),
    io:fwrite("~p~n",[Length_of_Message]),
    <<Length_of_Message/binary, 21, Message2/binary>>.

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
        

map_chunk_bulk({X,Y}) ->
    Message = <<26,1,1,0,0,0:16>>,
    {Chunk,Biome_data} = adamantum_chunk_generator:gen_column({0,0}),
    List_of_chunks = Chunk#db_chunk_column.chunks,
    Message2 = <<Message/binary, List_of_chunks/binary>>,
    Length_of_message = varint:encode_varint(byte_size(Message2)),
    <<Length_of_message/binary, Message2>>.







