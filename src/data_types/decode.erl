-module(decode).
-export([decode_type/2]).

decode_type(Data, Type) ->
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
%%        text_component ->
%%            decode_text_component(Data);
%%        json_text_component ->
%%            decode_json_text_component(Data);
        identifier ->
            decode_string(Data);
        varint ->
            decode_varint(Data);
%%        varlong ->
%%            decode_varlong(Data);
%%        entity_metadata ->
%%            decode_entity_metadata(Data);
%%        slot ->
%%            decode_slot(Data);
%%        hashed_slot ->
%%            decode_hashed_slot(Data);
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
            decode_fixed_bitset(Data)
%%        optional ->
%%            decode_optional(Data);
%%        prefixed_optional ->
%%            decode_prefixed_optional(Data);
%%        array ->
%%            decode_array(Data);
%%        prefixed_array ->
%%            decode_prefixed_array(Data);
%%        enum ->
%%            decode_enum(Data);
%%        byte_array ->
%%            decode_byte_array(Data);
%%        id_or_x ->
%%            decode_id_or_x(Data);
%%        id_set ->
%%            decode_id_set(Data);
%%        sound_event ->
%%            decode_sound_event(Data);
%%        chat_type ->
%%            decode_chat_type(Data);
%%        teleport_flags ->
%%            decode_teleport_flags(Data);
%%        recipe_display ->
%%            decode_recipe_display(Data);
%%        slot_display ->
%%            decode_slot_display(Data);
%%        light_data ->
%%            decode_light_data(Data);
%%        either_x_or_y ->
%%            decode_either_x_or_y(Data);
%%        game_profile ->
%%            decode_game_profile(Data);
%%        resolvable_profile ->
%%            decode_resolvable_profile(Data);
%%        debug_subscription_event ->
%%            decode_debug_subscription_event(Data);
%%        debug_subscription_update ->
%%            decode_debug_subscription_update(Data);
%%        lp_vec3 ->
%%            decode_lp_vec3(Data)
  end.
  
decode_bool(Data) ->
    <<Bool:8, Data2/binary>> = Data,
    {Data2, Bool =:= 1}.

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
    {Length, Data1} = decode_varint(Data),
    <<String:Length/binary, Data2/binary>> = Data1,
    String2 = binary_to_list(String),
    {Data2, String2}.

decode_varint(Data) when is_binary(Data) ->
    decode_varint(Data, 0, 0).

decode_varint(<<1:1, Number:7, Rest/binary>>, Position, Acc) ->
    decode_varint(Rest, Position + 7, (Number bsl Position) + Acc);
decode_varint(<<0:1, Number:7, Rest/binary>>, Position, Acc) ->
    {(Number bsl Position) + Acc, Rest};
decode_varint(<<>>, _, _) ->
    {error, "insufficient data"}.

decode_position(Data) ->
    <<X:26/signed-integer, Z:26/signed-integer, Y:12/signed-integer, Data2/binary>> = Data,
    {Data2, {X, Z, Y}}.
decode_uuid(Data) ->
    <<UUID:128/bitstring, Data2/binary>> = Data,
    {Data2, UUID}.

decode_bitset(Data) ->
    {Length, Data2} = decode_varint(Data),
    <<Bit_set:(Length*8)/signed-integer, Data3/binary>> = Data2,
    {Data3, Bit_set}.
decode_fixed_bitset(Data) ->
    {Length, Data2} = decode_varint(Data),
    <<Bit_set:Length/signed-integer, Data3/binary>> = Data2,
    {Data3, Bit_set}.