-module(encode).

-export([
    encode_type/2
]).

encode_type(Data, Type) ->
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
        varint ->
            encode_varint(Data);
        long ->
            encode_long(Data);
        float ->
            encode_float(Data);
        double ->
            encode_double(Data);
        string ->
            encode_string(Data);
%%        text_component ->
%%            encode_text_component(Data);
%%        json_text_component ->
%%            encode_json_text_component(Data);
        identifier ->
            encode_identifier(Data);
%%        varlong ->
%%            encode_varlong(Data);
%%        entity_metadata ->
%%            encode_entity_metadata(Data);
%%        slot ->
%%            encode_slot(Data);
%%        hashed_slot ->
%%            encode_hashed_slot(Data);
        nbt ->
            nbt:encode(Data);
        position ->
            encode_position(Data);
        angle ->
            encode_angle(Data);
        uuid ->
            encode_uuid(Data);
        bitset ->
            encode_bitset(Data);
        fixed_bitset ->
            encode_fixed_bitset(Data)
%%        optional ->
%%            encode_optional(Data);
%%        prefixed_optional ->
%%            encode_prefixed_optional(Data);
%%        array ->
%%            encode_array(Data);
%%        prefixed_array ->
%%            encode_prefixed_array(Data);
%%        enum ->
%%            encode_enum(Data);
%%        byte_array ->
%%            encode_byte_array(Data);
%%        id_or_x ->
%%            encode_id_or_x(Data);
%%        id_set ->
%%            encode_id_set(Data);
%%        sound_event ->
%%            encode_sound_event(Data);
%%        chat_type ->
%%            encode_chat_type(Data);
%%        teleport_flags ->
%%            encode_teleport_flags(Data);
%%        recipe_display ->
%%            encode_recipe_display(Data);
%%        slot_display ->
%%            encode_slot_display(Data);
%%        light_data ->
%%            encode_light_data(Data);
%%        either_x_or_y ->
%%            encode_either_x_or_y(Data);
%%        game_profile ->
%%            encode_game_profile(Data);
%%        resolvable_profile ->
%%            encode_resolvable_profile(Data);
%%        debug_subscription_event ->
%%            encode_debug_subscription_event(Data);
%%        debug_subscription_update ->
%%            encode_debug_subscription_update(Data);
%%        lp_vec3 ->
%%            encode_lp_vec3(Data)
    end.

encode_bool(true) -> <<1:8>>;
encode_bool(false) -> <<0:8>>;
encode_bool(1) -> <<1:8>>;
encode_bool(0) -> <<0:8>>.

encode_byte(Byte) when is_integer(Byte) ->
    <<Byte:8/signed-integer>>.

encode_ubyte(UByte) when is_integer(UByte) ->
    <<UByte:8/unsigned-integer>>.

encode_short(Short) when is_integer(Short) ->
    <<Short:16/signed-integer>>.

encode_ushort(UShort) when is_integer(UShort) ->
    <<UShort:16/unsigned-integer>>.

encode_int(Int) when is_integer(Int) ->
    <<Int:32/signed-integer>>.

encode_long(Long) when is_integer(Long) ->
    <<Long:64/signed-integer>>.

encode_float(Float) when is_number(Float) ->
    <<Float:32/float>>.

encode_double(Double) when is_number(Double) ->
    <<Double:64/float>>.

encode_string(String) when is_list(String) ->
    Bin = list_to_binary(String),
    LenBin = encode_varint(byte_size(Bin)),
    <<LenBin/binary, Bin/binary>>;
encode_string(String) when is_binary(String) ->
    LenBin = encode_varint(byte_size(String)),
    <<LenBin/binary, String/binary>>.

encode_varint(I) when is_integer(I), I < 0 ->
    encode_varint(I band 16#FFFFFFFF);
encode_varint(I) when is_integer(I), I >= 0, I =< 127 ->
    <<I:8>>;
encode_varint(I) when is_integer(I), I > 127 ->
    <<1:1, (I band 127):7, (encode_varint(I bsr 7))/binary>>.

encode_identifier(String) ->
    encode_string(String).

encode_position({X, Z, Y}) when is_integer(X), is_integer(Z), is_integer(Y) ->
    <<X:26/signed-integer, Z:26/signed-integer, Y:12/signed-integer>>.

encode_angle(Angle) when is_integer(Angle) ->
    encode_byte(Angle).

encode_uuid(<<UUID:128/bitstring>>) ->
    <<UUID:128/bitstring>>;
encode_uuid(UUID) when is_integer(UUID) ->
    <<UUID:128/unsigned-integer>>.

encode_bitset({Length, BitSet}) when is_integer(Length), is_integer(BitSet) ->
    LenBin = encode_varint(Length),
    <<LenBin/binary, BitSet:(Length*8)/signed-integer>>;
encode_bitset(BitSet) when is_integer(BitSet) ->
    Length = calc_bitset_bytes(BitSet),
    LenBin = encode_varint(Length),
    <<LenBin/binary, BitSet:(Length*8)/signed-integer>>.

encode_fixed_bitset({Bits, BitSet}) when is_integer(Bits), is_integer(BitSet) ->
    LenBin = encode_varint(Bits),
    <<LenBin/binary, BitSet:Bits/signed-integer>>;
encode_fixed_bitset(BitSet) when is_integer(BitSet) ->
    Bits = calc_fixed_bitset_bits(BitSet),
    LenBin = encode_varint(Bits),
    <<LenBin/binary, BitSet:Bits/signed-integer>>.

calc_bitset_bytes(Val) when Val >= 0 ->
    calc_bitset_bytes(Val, 1);
calc_bitset_bytes(Val) when Val < 0 ->
    calc_bitset_bytes_neg(Val, 1).

calc_bitset_bytes(Val, Bytes) ->
    Max = (1 bsl (Bytes * 8 - 1)) - 1,
    if Val =< Max -> Bytes;
       true -> calc_bitset_bytes(Val, Bytes + 1)
    end.

calc_bitset_bytes_neg(Val, Bytes) ->
    Min = -(1 bsl (Bytes * 8 - 1)),
    if Val >= Min -> Bytes;
       true -> calc_bitset_bytes_neg(Val, Bytes + 1)
    end.

calc_fixed_bitset_bits(Val) when Val >= 0 ->
    calc_fixed_bitset_bits(Val, 8);
calc_fixed_bitset_bits(Val) when Val < 0 ->
    calc_fixed_bitset_bits_neg(Val, 8).

calc_fixed_bitset_bits(Val, Bits) ->
    Max = (1 bsl (Bits - 1)) - 1,
    if Val =< Max -> Bits;
       true -> calc_fixed_bitset_bits(Val, Bits + 8)
    end.

calc_fixed_bitset_bits_neg(Val, Bits) ->
    Min = -(1 bsl (Bits - 1)),
    if Val >= Min -> Bits;
       true -> calc_fixed_bitset_bits_neg(Val, Bits + 8)
    end.

