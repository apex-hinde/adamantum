-module(adamantum_chunk_generator).
-include("records.hrl").
-export([gen_column/2]).

gen_column(X, Y) ->
    EmptyChunk = binary:copy(<<0:4, 0:4, 15:8>>,16*16*16)
%                             metadata=binary:copy(<<0>>,16*16*8),
%                             block_light=binary:copy(<<0>>,16*16*8),
%                             sky_light=binary:copy(<<0>>,16*16*8)
                        ,
    BedrockChunk = list_to_binary([binary:copy(<<7:4,0:4,0:8>>,256),
                                                               binary:copy(<<1:4,0:4,0:8>>,256*15)]),
    StoneChunk = binary:copy(<<1:4,0:4,0:8>>,16*16*16),
    TopChunk = list_to_binary([binary:copy(<<3:4,0:4, 0:8>>,256*15),
                                                           binary:copy(<<2:4,15:4, 15:8>>,256)]),


    #chunk_column{full_column=true,
                       chunks = <<BedrockChunk/binary,
                               StoneChunk/binary,
                               StoneChunk/binary,
                               StoneChunk/binary,
                               TopChunk/binary,
                               EmptyChunk/binary,
                               EmptyChunk/binary,
                               EmptyChunk/binary,
                               EmptyChunk/binary,
                               EmptyChunk/binary,
                               EmptyChunk/binary,
                               EmptyChunk/binary,
                               EmptyChunk/binary,
                               EmptyChunk/binary,
                               EmptyChunk/binary,
                               EmptyChunk/binary>>,
                       biome=binary:copy(<<0>>,256)}.


    


