-module(adamantum_encode).
-export([encode_chunk_bulk/4]).
-include("records.hrl").

encode_chunk_bulk([], [], Acc_chunks, Acc_array) ->
    <<Acc_array/binary, Acc_chunks/binary>>;

encode_chunk_bulk([], Chunks_to_send, Acc_chunks, Acc_array) ->
    [{X,Y}|T] = Chunks_to_send,
    Chunk_data = adamantum_chunk_manager:get_chunk_column({X,Y}),
    Chunk_column = decode_chunk_records(Chunk_data#db_chunk_column.chunks, <<>>),
    Biome_data = Chunk_data#db_chunk_column.biome,
    Chunk = <<Chunk_column/binary, Biome_data/binary>>,
    Acc_chunks2 = <<Acc_chunks/binary, Chunk/binary>>,

    Bit_map2 = <<16:16>>,
    Acc_array2 = <<Acc_array/binary, X:32, Y:32, Bit_map2/binary>>,
    encode_chunk_bulk([], T, Acc_chunks2, Acc_array2);

encode_chunk_bulk(Chunks_to_remove, Chunks_to_send, Acc_chunks, Acc_array) ->
    [{X,Y}|T] = Chunks_to_remove,
    Acc_chunks2 = <<Acc_chunks/binary, 0:256>>,
    Acc_array2 = <<Acc_array/binary, X:32, Y:32, 0:16>>,
    encode_chunk_bulk(T, Chunks_to_send, Acc_chunks2, Acc_array2).

decode_chunk_records([], Acc) ->
    Acc;

decode_chunk_records([Chunk|T], Acc) ->
    Block_type = Chunk#db_chunk.block_type,
    Block_light = Chunk#db_chunk.block_light,
    Sky_light = Chunk#db_chunk.sky_light,
    Acc2 = <<Acc/binary, Block_type/binary, Block_light/binary, Sky_light/binary>>,
    decode_chunk_records(T, Acc2).


    


