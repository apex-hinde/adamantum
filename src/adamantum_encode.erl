-module(adamantum_encode).
-export([login_success/3, set_compression/0,join_game/1, spawn_position/3, player_position_and_look/6]).

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
    Level_type = <<"default">>,
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
    






