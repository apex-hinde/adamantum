-module(adamantum_decode).
-export([decode_message/2, decode_bool/1]).

decode_message(Data, Packet_ID) ->
    {Packet_ID, _,Param_list} = data_packets:get_by_packet_id_serverbound(Packet_ID),
    Data2 = decode_message_list(Data, Param_list, []),
    Data2.


decode_message_list(<<>>, [], Acc) ->
    lists:reverse(Acc);

decode_message_list(_Data, [], _Acc) ->
    io:format("~p~n", ["decode error"]);

decode_message_list(Data, [H|T], Acc) ->
    io:format("~p~n", [Data]),
    io:format("~p~n", [H]),
    {Data2, Result} = get_value(Data, H),

    decode_message_list(Data2, T, [Result|Acc]).


% need to add array of x, optional x and enum x decode varlong
get_value(Data, Type) ->
    case Type of
        bool ->
            decode_bool(Data);
        byte ->
            decode_byte(Data);
        ubyte ->
            decode_ubyte(Data);
        short ->
            decode_short(Data);
        ushort ->
            decode_ushort(Data);
        int ->
            decode_int(Data);
        long ->
            decode_long(Data);
        float -> 
            decode_float(Data);
        double ->
            decode_double(Data);
        string ->
            decode_string(Data);
        chat ->
            decode_chat(Data);
        identifier ->
            decode_identifier(Data);
        varint ->
            decode_varint(Data);
%        varlong ->
%            decode_varlong(Data);
%        chunk_data ->
%           decode_chunk(Data);
%        enitity_metadata ->
%           decode_entity_metadata(Data);
%        slot ->
%            decode_slot(Data);
%        nbt_tag ->
%            decode_nbt_tag(Data);
        position ->
            decode_position(Data);
        angle ->
            decode_angle(Data);
        uuid ->
            decode_uuid(Data)

    end.

decode_bool(Data) ->
    <<Bool:8, Data2/binary>> = Data,
    {Data2, Bool =:= <<1>>}.
decode_byte(Data) ->
    <<Byte:8/signed-integer, Data2/binary>> = Data,
    {Data2, Byte}.
decode_ubyte(Data) ->
    <<UByte:8/unsigned-integer, Data2/binary>> = Data,
    {Data2, UByte}.
decode_short(Data) ->
    <<Short:16/signed-integer, Data2/binary>> = Data,
    {Data2, Short}.
decode_ushort(Data) ->
    <<UShort:16/unsigned-integer, Data2/binary>> = Data,
    {Data2, UShort}.
decode_int(Data) ->
    <<Int:32/signed-integer, Data2/binary>> = Data,
    {Data2, Int}.
decode_long(Data) ->
    <<Long:64/signed-integer, Data2/binary>> = Data,
    {Data2, Long}.
decode_float(Data) ->
    <<Float:32/float, Data2/binary>> = Data,
    {Data2, Float}.
decode_double(Data) ->
    <<Double:64/float, Data2/binary>> = Data,
    {Data2, Double}.
decode_string(Data) ->
    {Length, Data1} = varint:decode_varint(Data),
    <<String:Length/binary, Data2/binary>> = Data1,
    {Data2, String}.
decode_chat(Data) ->
    decode_string(Data).
decode_identifier(Data) ->
    decode_string(Data).
decode_varint(Data) ->
    varint:decode_varint(Data).
decode_position(Data) ->
    <<X:26/signed-integer, Y:12/signed-integer, Z:26/signed-integer, Data2/binary>> = Data,
    {Data2, {X, Y, Z}}.
decode_angle(Data) ->
    <<Angle:8/unsigned-integer, Data2/binary>> = Data,
    {Data2, Angle}.
decode_uuid(Data) ->
    <<UUID:128/binary, Data2/binary>> = Data,
    {Data2, UUID}.