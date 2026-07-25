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
        varlong ->
            encode_varlong(Data);
%%        entity_metadata ->
%%            encode_entity_metadata(Data);
        slot ->
            encode_slot(Data);
        hashed_slot ->
            encode_hashed_slot(Data);
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
            encode_fixed_bitset(Data);
        {optional, Inner_Type, Bool} ->
            encode_optional(Data, Inner_Type, Bool);
        {optional, Inner_Type} ->
            encode_prefixed_optional(Data, Inner_Type);
        {prefixed_optional, Inner_Type} ->
            encode_prefixed_optional(Data, Inner_Type);

        {array, ElemType} ->
            encode_array(Data, ElemType);
        {array, Arg1, Arg2} ->
            encode_array(Data, Arg1, Arg2);
        {prefixed_array, ElemType} ->
            encode_prefixed_array(Data, ElemType);
        {prefixed_array, PrefixType, ElemType} ->
            encode_prefixed_array(Data, PrefixType, ElemType);
        enum ->
            encode_enum(Data);
        {enum, Arg1} ->
            encode_enum(Data, Arg1);
        {enum, Arg1, Arg2} ->
            encode_enum(Data, Arg1, Arg2);
        byte_array ->
            encode_byte_array(Data);
        {byte_array, Arg1} ->
            encode_byte_array(Data, Arg1)
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

encode_varlong(I) when is_integer(I), I < 0 ->
    encode_varlong(I band 16#FFFFFFFFFFFFFFFF);
encode_varlong(I) when is_integer(I), I >= 0, I =< 127 ->
    <<I:8>>;
encode_varlong(I) when is_integer(I), I > 127 ->
    <<1:1, (I band 127):7, (encode_varlong(I bsr 7))/binary>>.

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

encode_byte_array(Data) when is_binary(Data) ->
    Data;
encode_byte_array(Data) when is_list(Data) ->
    list_to_binary(Data).

encode_byte_array(Data, Length) when is_integer(Length) ->
    Bin = if is_binary(Data) -> Data; true -> list_to_binary(Data) end,
    <<ByteArray:Length/binary, _/binary>> = Bin,
    ByteArray;
encode_byte_array(Data, PrefixType) when is_atom(PrefixType) ->
    Bin = if is_binary(Data) -> Data; true -> list_to_binary(Data) end,
    LenBin = encode_type(byte_size(Bin), PrefixType),
    <<LenBin/binary, Bin/binary>>.


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

encode_optional({some, Value}, Type, true) ->
    encode_type(Value, Type);
encode_optional(Value, Type, true) ->
    encode_type(Value, Type);
encode_optional(_Value, _Type, false) ->
    <<>>.

encode_prefixed_optional({some, Value}, InnerType) ->
    <<(encode_bool(true))/binary, (encode_type(Value, InnerType))/binary>>;
encode_prefixed_optional(none, _InnerType) ->
    encode_bool(false);
encode_prefixed_optional(undefined, _InnerType) ->
    encode_bool(false);
encode_prefixed_optional(Value, InnerType) ->
    <<(encode_bool(true))/binary, (encode_type(Value, InnerType))/binary>>.

encode_array(List, ElemType) when is_list(List) ->
    encode_array_loop(List, ElemType, <<>>);
encode_array(List, {Count, ElemType}) when is_list(List), is_integer(Count) ->
    encode_array_loop(lists:sublist(List, Count), ElemType, <<>>);
encode_array(List, {ElemType, Count}) when is_list(List), is_integer(Count) ->
    encode_array_loop(lists:sublist(List, Count), ElemType, <<>>).

encode_array(List, Count, ElemType) when is_list(List), is_integer(Count) ->
    encode_array_loop(lists:sublist(List, Count), ElemType, <<>>);
encode_array(List, ElemType, Count) when is_list(List), is_integer(Count) ->
    encode_array_loop(lists:sublist(List, Count), ElemType, <<>>).

encode_array_loop([], _ElemType, Acc) ->
    Acc;
encode_array_loop([Head | Tail], ElemType, Acc) ->
    ElemBin = encode_type(Head, ElemType),
    encode_array_loop(Tail, ElemType, <<Acc/binary, ElemBin/binary>>).

encode_prefixed_array({prefixed_array, List}, ElemType) ->
    encode_prefixed_array(List, ElemType);
encode_prefixed_array(List, ElemType) when is_list(List) ->
    LenBin = encode_varint(length(List)),
    ArrayBin = encode_array_loop(List, ElemType, <<>>),
    <<LenBin/binary, ArrayBin/binary>>.

encode_prefixed_array({prefixed_array, List}, PrefixType, ElemType) ->
    encode_prefixed_array(List, PrefixType, ElemType);
encode_prefixed_array(List, PrefixType, ElemType) when is_list(List) ->
    LenBin = encode_type(length(List), PrefixType),
    ArrayBin = encode_array_loop(List, ElemType, <<>>),
    <<LenBin/binary, ArrayBin/binary>>.

encode_enum(Data) ->
    encode_enum(Data, varint).

encode_enum(Data, InnerType) when is_atom(InnerType) ->
    encode_type(Data, InnerType);
encode_enum(Data, EnumList) when is_list(EnumList); is_map(EnumList) ->
    encode_enum(Data, varint, EnumList).

encode_enum(Data, InnerType, EnumList) when is_list(EnumList) ->
    case is_integer(Data) of
        true ->
            encode_type(Data, InnerType);
        false ->
            case find_index(Data, EnumList, 0) of
                {ok, Idx} ->
                    encode_type(Idx, InnerType);
                error ->
                    error({invalid_enum, Data})
            end
    end;
encode_enum(Data, InnerType, EnumMap) when is_map(EnumMap) ->
    case EnumMap of
       #{Data := Val} ->
           encode_type(Val, InnerType);
       #{} ->
           case is_integer(Data) of
                true -> encode_type(Data, InnerType);
                false -> error({invalid_enum, Data})
            end
   end.

find_index(_Elem, [], _Idx) ->
    error;
find_index(Elem, [Elem | _Rest], Idx) ->
    {ok, Idx};
find_index(Elem, [_Head | Rest], Idx) ->
    find_index(Elem, Rest, Idx + 1).

%% Slot
%%
%% Wire format (workaround for opaque component data):
%%   ItemCount        :: VarInt
%%   [if ItemCount > 0]
%%     ItemID         :: VarInt
%%     NAdd           :: VarInt
%%     NRemove        :: VarInt
%%     ComponentsToAdd    :: NAdd   × {TypeId::VarInt, DataLen::VarInt, Data::binary}
%%     ComponentsToRemove :: NRemove × TypeId::VarInt
%%
%% NOTE: DataLen + Data is NOT the real Minecraft wire format for component data
%% (which is type-dependent). It is a local workaround until the component-type
%% registry and individual component codecs are implemented.
encode_slot(empty) ->
    encode_varint(0);
encode_slot(0) ->
    encode_varint(0);
encode_slot({ItemCount, ItemID, ComponentsToAdd, ComponentsToRemove}) ->
    ItemCountBin = encode_varint(ItemCount),
    ItemIDBin    = encode_varint(ItemID),
    NAddBin      = encode_varint(length(ComponentsToAdd)),
    NRemoveBin   = encode_varint(length(ComponentsToRemove)),
    AddBin       = encode_slot_add_components(ComponentsToAdd, <<>>),
    RemoveBin    = encode_array_loop(ComponentsToRemove, varint, <<>>),
    <<ItemCountBin/binary, ItemIDBin/binary, NAddBin/binary, NRemoveBin/binary,
      AddBin/binary, RemoveBin/binary>>.

encode_slot_add_components([], Acc) -> Acc;
encode_slot_add_components([{TypeId, DataBin} | Rest], Acc) ->
    TypeIdBin  = encode_varint(TypeId),
    DataLenBin = encode_varint(byte_size(DataBin)),
    encode_slot_add_components(Rest,
        <<Acc/binary, TypeIdBin/binary, DataLenBin/binary, DataBin/binary>>).

%% Hashed Slot
%%
%% Wire format (matches real Minecraft protocol for hashed slots):
%%   HasItem          :: Boolean
%%   [if HasItem]
%%     ItemID         :: VarInt
%%     ItemCount      :: VarInt
%%     NAdd           :: VarInt
%%     ComponentsToAdd    :: NAdd   × {TypeId::VarInt, Hash::Int32}
%%     NRemove        :: VarInt
%%     ComponentsToRemove :: NRemove × TypeId::VarInt
%%
%% The Hash is a CRC32C checksum of the component data (currently undocumented).
encode_hashed_slot(empty) ->
    encode_bool(false);
encode_hashed_slot({ItemID, ItemCount, ComponentsToAdd, ComponentsToRemove}) ->
    HasItemBin    = encode_bool(true),
    ItemIDBin     = encode_varint(ItemID),
    ItemCountBin  = encode_varint(ItemCount),
    NAddBin       = encode_varint(length(ComponentsToAdd)),
    AddBin        = encode_hashed_slot_add_components(ComponentsToAdd, <<>>),
    NRemoveBin    = encode_varint(length(ComponentsToRemove)),
    RemoveBin     = encode_array_loop(ComponentsToRemove, varint, <<>>),
    <<HasItemBin/binary, ItemIDBin/binary, ItemCountBin/binary,
      NAddBin/binary, AddBin/binary, NRemoveBin/binary, RemoveBin/binary>>.

encode_hashed_slot_add_components([], Acc) -> Acc;
encode_hashed_slot_add_components([{TypeId, Hash} | Rest], Acc) ->
    TypeIdBin = encode_varint(TypeId),
    HashBin   = <<Hash:32/signed-integer>>,
    encode_hashed_slot_add_components(Rest,
        <<Acc/binary, TypeIdBin/binary, HashBin/binary>>).
