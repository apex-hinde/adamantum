-module(adamantum_decode).
-export([decode_message/2, encode_message/2]).
-include("records.hrl").

decode_message(Data, Packet_name) ->

    {Packet_name, Param_list} = data_packets:get_messages_serverbound(Packet_name),
    Data2 = decode_message_list(Data, Param_list,  []),
    Data2.
decode_message_list(<<>>, [], Acc) ->
    lists:reverse(Acc);

decode_message_list(_Data, [], _Acc) ->
    io:format("~p~n", ["decode error"]);

decode_message_list(Data, [H|T],  Acc) ->
%    io:format("Data: ~p~n", [Data]),
%    io:format("H: ~p~n", [H]),

    {Data2, Result} = get_decode_value(Data, H),

    decode_message_list(Data2, T, [Result|Acc]).


% need to add array of x, optional x and enum x decode varlong
get_decode_value(Data, Type) ->
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
            decode_uuid(Data);
        byte_array_plugin ->
            decode_byte_array_plugin(Data) %a list of bytes only for decoding message 23

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
    {Result, Data2} = varint:decode_varint(Data),
    {Data2, Result}.
decode_position(Data) ->
    <<X:26/signed-integer, Y:12/signed-integer, Z:26/signed-integer, Data2/binary>> = Data,
    {Data2, {X, Y, Z}}.
decode_angle(Data) ->
    <<Angle:8/unsigned-integer, Data2/binary>> = Data,
    {Data2, Angle}.
decode_uuid(Data) ->
    <<UUID:128/binary, Data2/binary>> = Data,
    {Data2, UUID}.

decode_byte_array_plugin(Data) ->
    {<<>>, Data}.
encode_message(Data, Packet_name) ->
    io:format("Data: ~p~n", [Data]),
    {Packet_name, Param_list} = data_packets:get_messages_clientbound(Packet_name),
    Data2 = encode_message_list(Data, Param_list,  <<>>),
    
    Packet_ID = data_packets:get_packet_number_clientbound(Packet_name),
    Data3 = <<Packet_ID:8, Data2/binary>>,

    {Packet_name, Data3}.
encode_message_list([], [], Acc) ->
    Acc;

encode_message_list([Data|T_data], [H|T],  Acc) ->
    Result = get_encode_value(Data, H),
    encode_message_list(T_data, T, <<Acc/binary, Result/binary>>).
get_encode_value(Data, Type) ->
    case Type of
        bool ->
            encode_bool(Data);
        byte ->
            encode_byte(Data);
        ubyte ->
            encode_ubyte(Data);
        short ->
            encode_short(Data);
        ushort ->
            encode_ushort(Data);
        int ->
            encode_int(Data);
        long ->
            encode_long(Data);
        float -> 
            encode_float(Data);
        double ->
            encode_double(Data);
        string ->
            encode_string(Data);
        chat ->
            encode_chat(Data);
        identifier ->
            encode_identifier(Data);
        varint ->
            encode_varint(Data);
%        varlong ->
%            encode_varlong(Data);
%        chunk ->
%           encode_chunk(Data);
%        enitity_metadata ->
%           encode_entity_metadata(Data);
%        slot ->
%            encode_slot(Data);
%        nbt_tag ->
%            encode_nbt_tag(Data);
        position ->
            encode_position(Data);
        angle ->
            encode_angle(Data);
        uuid ->
            encode_uuid(Data)
%        byte_array_plugin ->
%            encode_byte_array_plugin(Data) %a list of bytes only for decoding message 23

        end.

encode_bool(Data) ->
    case Data of
        true ->
            <<1>>;
        false ->
            <<0>>
    end.

encode_byte(Data) ->
    <<Data:8/signed>>.

encode_ubyte(Data) ->
    <<Data:8/unsigned>>.

encode_short(Data) ->
    <<Data:16/signed>>.

encode_ushort(Data) ->
    <<Data:16/unsigned>>.

encode_int(Data) ->
    <<Data:32/signed>>.

encode_long(Data) ->
    <<Data:64/signed>>.

encode_float(Data) ->
    <<Data:32/float>>.

encode_double(Data) ->
    <<Data:64/float>>.

encode_string(Data) ->
    Length = length(Data),
    Length_of_string = varint:encode_varint(Length),
    Data2 = list_to_binary(Data),
    <<Length_of_string/binary, Data2/binary>>.

encode_chat(Data) ->
    encode_string(Data).

encode_identifier(Data) ->
    encode_string(Data).

encode_varint(Data) ->
    varint:encode_varint(Data).




encode_position({X,Y,Z}) ->
    <<X:26/signed, Y:12/signed, Z:26/signed>>.

encode_angle(Data) ->
    <<Data:8/unsigned>>.

encode_uuid(Data) ->
    <<Data:128/unsigned>>.




