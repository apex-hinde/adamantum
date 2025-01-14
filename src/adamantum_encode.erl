-module(adamantum_encode).
-export([login_success/3, set_compression/0,join_game/1, spawn_position/3, player_position_and_look/6, chunk_data/2,
        map_chunk_bulk/2]).
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
    io:fwrite("~p~n", [Message]),
    Length_of_message = varint:encode_varint(byte_size(Message)),
    <<Length_of_message/binary, Message/binary>>.


player_position_and_look(X, Z, Y, Yaw, Pitch, Reletive) ->
   Message =  <<8, X:64/float, Y:64/float, Z:64/float, Yaw:32/float, Pitch:32/float, Reletive>>,
   Length_of_message = varint:encode_varint(byte_size(Message)),
   io:fwrite("~p~n", [Message]),
   <<Length_of_message/binary, Message/binary>>.
    

chunk_data(X,Y) ->

    Chunk_x = floor(X/16),
    Chunk_y = floor(Y/16),
    Message = <<Chunk_x:32,Chunk_y:32,1,1:16>>,
    Chunk = list_to_binary([binary:copy(<<0:4, 7:4>>,256)]),
%    List_of_chunks = Chunk#chunk_column.chunks,
    Length_of_data = varint:encode_varint(byte_size(Chunk)),
    Message2 = <<21, Message/binary, Length_of_data/binary, Chunk/binary>>,
    Length_of_Message = varint:encode_varint(byte_size(Message2)),
    io:format("~p~n", [<<Length_of_Message/binary, Message2/binary>>]),
    <<Length_of_Message/binary, 21, Message2/binary>>.


map_chunk_bulk(X,Y) ->
    Message = <<26, 1, 1,0,0,0:16>>,
    Chunk = adamantum_chunk_generator:gen_column(0,0),
    List_of_chunks = Chunk#chunk_column.chunks,
    Message2 = <<Message/binary, List_of_chunks/binary>>,
    Length_of_message = varint:encode_varint(byte_size(Message2)),
    <<Length_of_message/binary, Message2>>.




%build_chunk_map(0,0,_) ->
%    [];
%build_chunk_map(X, Y, Acc) ->
%    ;
%build_chunk_map(, Y, Arg3)




