-module(decode).
-export([decode_message/2]).
%% exported for testing reasons
-export([decode_uuid/1, decode_login_success/1]).



decode_message(Data, Packet_name) ->
    {_, Param_list} = data_packets:get_messages_serverbound(Packet_name),
    Data2 = decode_message_list(Data, Param_list,  []),
    io:format("~p~n", [Data2]),
    Data2.

decode_message_list(<<>>, [], Acc) ->
    lists:reverse(Acc);

decode_message_list(Data, [], Acc) ->
    {Data, Acc};
decode_message_list(Data, [H|T],  Acc) ->

    {Data2, Result} = get_decode_value(Data, H),
    decode_message_list(Data2, T, [Result|Acc]).

get_decode_value(Data, Type) ->
    case Type of 
        {optional, Type2} ->
            if length(Data) >= 1 ->
                get_decode_value(Data, Type2)
            end;
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
        nbt ->
            nbt:decode(Data);
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

        {prefixed_optional, Type2} ->
            decode_prefixed_optional(Data, Type2);

%% after this is for specific cases
%% this is if the byte array contains all the rest of the data

        byte_array_end ->
            decode_byte_array_end(Data);
        byte_array_chat ->
            decode_byte_array_chat(Data);
        {prefixed_array, Type2} ->
            decode_prefixed_array(Data, Type2);

%% custom
        interact ->
            decode_interact(Data)
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
    {Data2, UUID}.
decode_bitset(Data) ->
    {Length, Data2} = varint:decode_varint(Data),
    <<Bit_set:(Length*8)/signed-integer, Data3/binary>> = Data2,
    {Data3, Bit_set}.
decode_fixed_bitset(Data) ->
    {Length, Data2} = varint:decode_varint(Data),
    <<Bit_set:Length/signed-integer, Data3/binary>> = Data2,
    {Data3, Bit_set}.

decode_prefixed_optional(Data, Type) ->
    <<Bool:8, Data2/binary>> = Data,
    case Bool of
        0 ->
            {Data2, undefined};
        1 ->
            get_decode_value(Data2, Type)
    end.
decode_byte_array_end(Data) ->
    {<<>>, Data}.
decode_byte_array_chat(Data) ->
    <<Data2:256/binary, Rest/binary>> = Data,
    {Rest, Data2}.

decode_prefixed_array(Data, Types) ->
    {Length, Data2} = varint:decode_varint(Data),
    decode_prefixed_array(Data2, Types, Length, []).

decode_prefixed_array(Data, _Types, 0, [Acc]) ->

    {Data, lists:reverse(Acc)};

decode_prefixed_array(Data, Types, Length, Acc) ->
    case decode_message_list(Data, Types, []) of
        {Rest, Result} ->
            decode_prefixed_array(Rest, Types, Length-1, [Result|Acc]);
        Result ->
            decode_prefixed_array(<<>>, Types, Length-1, [Result|Acc])
    end.



decode_interact(Data) ->
    {Type, Data2} = varint:decode_varint(Data),
    case Type of
        2->
            {Data3, Target_X} = decode_float(Data2),
            {Data4, Target_Y} = decode_float(Data3),
            {Data5, Target_Z} = decode_float(Data4),
            {Hand, Data6} = varint:decode_varint(Data5),
            {Data6, {Type, Target_X, Target_Y, Target_Z, Hand}};
        _->
            {Data2, Type}
    end.

    






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
