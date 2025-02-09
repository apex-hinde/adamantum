-module(decode).
-export([decode_message/2]).
-export([decode_uuid/1, decode_login_success/1]).



decode_message(Data, Packet_name) ->

    {Packet_name, Param_list} = data_packets:get_messages_serverbound(Packet_name),
    Data2 = decode_message_list(Data, Param_list,  []),
    Data2.

decode_message_list(<<>>, [], Acc) ->
    lists:reverse(Acc);

decode_message_list(_Data, [], _Acc) ->
    io:format("~p~n", ["decode error"]);

decode_message_list(Data, [H|T],  Acc) ->

    {Data2, Result} = get_decode_value(Data, H),
    decode_message_list(Data2, T, [Result|Acc]).

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
%        text_component ->
%            decode_text_component(Data);
%        json_text_component ->
%           decode_json_text_component(Data);
        identifier ->
            decode_string(Data);
        varint ->
            decode_varint(Data);
%        varlong ->
%            decode_varlong(Data);
%        entity_metadata ->
%            decode_entity_metadata(Data);
%        slot ->
%            decode_slot(Data);
%        nbt ->
%            decode_nbt(Data);
        position ->
            decode_position(Data);
        angle ->
            decode_byte(Data);
        uuid ->
            decode_uuid(Data);
        bitset ->
            decode_bitset(Data);
        fixed_bitset ->
            decode_fixed_bitset(Data);
        prefixed_array ->
            decode_prefixed_array(Data)
%        in_set ->
%            decode_in_set(Data);
%        sound_event ->
%            decode_sound_event(Data);
%        teleport_flags ->
%            decode_teleport_flags(Data);
%        recipe_display ->
%            decode_recipe_display(Data);
%        slot_display ->
%            decode_slot_display(Data);
%        chunk_data ->
%            decode_chunk_data(Data);
%        light_data ->
%            decode_light_data(Data)
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
    String2 = binary_to_list(String),
    {Data2, String2}.
decode_varint(Data) ->
    {Number, Data2} = varint:decode_varint(Data),
    {Data2,Number}.
decode_position(Data) ->
    <<X:26/signed-integer, Z:26/signed-integer, Y:12/signed-integer, Data2/binary>> = Data,
    {Data2, {X, Z, Y}}.
decode_uuid(Data) ->
    <<UUID:128/bitstring, Data2/binary>> = Data,
    io:format("uuid ~p~n", [UUID]),
    {Data2, UUID}.
decode_bitset(Data) ->
    {Length, Data2} = varint:decode_varint(Data),
    <<Bit_set:(Length*8)/signed-integer, Data3/binary>> = Data2,
    {Data3, Bit_set}.
decode_fixed_bitset(Data) ->
    {Length, Data2} = varint:decode_varint(Data),
    <<Bit_set:Length/signed-integer, Data3/binary>> = Data2,
    {Data3, Bit_set}.
decode_prefixed_array(Data) ->
    {Length, Data2} = varint:decode_varint(Data),
    <<Prefixed_array:Length, Data3/binary>> = Data2,
    {Data3, Prefixed_array}.





% only for tests



decode_login_success(Data) ->
    {Length_overall, Rest} = varint:decode_varint(Data),
    {Packet_id, Rest2} = varint:decode_varint(Rest),
    {Rest3, UUID} = decode_uuid(Rest2),
    {Rest4, Username} = decode_string(Rest3),
    {Length_of_array, Rest5} = varint:decode_varint(Rest4),
    {Rest6, Name} = decode_string(Rest5),
    {Rest7, Value} = decode_string(Rest6),
    {_, Bool} = decode_bool(Rest7),
    {Length_overall, Packet_id, UUID, Username, Length_of_array, Name, Value, Bool}.
