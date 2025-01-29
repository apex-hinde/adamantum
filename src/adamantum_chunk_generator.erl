-module(adamantum_chunk_generator).

-include("records.hrl").

-export([gen_column/0]).

gen_column() ->
    Empty_chunk = #db_chunk{block_type = binary:copy(<<0:16>>, 4096), %4096
                            block_light = binary:copy(<<0>>, 2048), %2048   
                            sky_light = binary:copy(<<0>>, 2048)}, %2048

    Bedrock_chunk = Empty_chunk#db_chunk{block_type = list_to_binary([binary:copy(<<7:4, 0:12>>, 256),binary:copy(<<1:4, 0:12>>, 3840)])}, %256, 3840
    Stone_chunk = Empty_chunk#db_chunk{block_type = list_to_binary([binary:copy(<<1:4, 0:12>>, 4096)])}, %4096
    Top_chunk =
        Empty_chunk#db_chunk{block_type = list_to_binary([binary:copy(<<3:4, 0:12>>, 3840), binary:copy(<<2:4, 0:12>>, 256)])}, %3840, 256

    #db_chunk_column{full_column = true,
                     chunks = [Bedrock_chunk,
                                 Stone_chunk,
                                 Stone_chunk,
                                 Top_chunk,
                                 Empty_chunk,
                                 Empty_chunk,
                                 Empty_chunk,
                                 Empty_chunk,
                                 Empty_chunk,
                                 Empty_chunk,
                                 Empty_chunk,
                                 Empty_chunk,
                                 Empty_chunk,
                                 Empty_chunk,
                                 Empty_chunk,
                                 Empty_chunk
                                ],
                     biome = binary:copy(<<0>>, 256)}.


        


