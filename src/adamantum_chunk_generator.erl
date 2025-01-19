-module(adamantum_chunk_generator).

-include("records.hrl").

-export([gen_column/0]).

gen_column() ->
    Empty_chunk = #db_chunk{type = binary:copy(<<0>>, 4096), %4096
                        metadata = binary:copy(<<0>>, 2048), %2048
                        block_light = binary:copy(<<0>>, 2048), %2048   
                        sky_light = binary:copy(<<0>>, 2048)}, %2048

    Bedrock_chunk = Empty_chunk#db_chunk{type = list_to_binary([binary:copy(<<7>>, 256),binary:copy(<<1>>, 4096)])}, %256, 4096
 

    Stone_chunk = Empty_chunk#db_chunk{type = list_to_binary([binary:copy(<<1>>, 4096)])}, %4096
    Top_chunk =
        Empty_chunk#db_chunk{type = list_to_binary([binary:copy(<<3>>, 3840), binary:copy(<<2>>, 256)])}, %3840, 256

    #db_chunk_column{full_column = true,
                  chunks = [{ Bedrock_chunk,
                              Stone_chunk,
                              Top_chunk,
                              Empty_chunk}],
                  biome = binary:copy(<<0>>, 256)}.


        


