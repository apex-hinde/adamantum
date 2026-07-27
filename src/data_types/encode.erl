-module(encode).
-include("src/data_types/records.hrl").

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
        text_component ->
            encode_text_component(Data);
        json_text_component ->
            encode_json_text_component(Data);
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
        {prefixed_array, ElemList} when is_list(ElemList) ->
            encode_prefixed_array_list(Data, ElemList);
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
            encode_byte_array(Data, Arg1);
        id_or_x ->
            encode_id_or_x(Data);
        {id_or_x, InnerType} ->
            encode_id_or_x(Data, InnerType);
        id_set ->
            encode_id_set(Data);
        {id_set, _Arg} ->
            encode_id_set(Data);
        sound_event ->
            encode_sound_event(Data);
        teleport_flags ->
            encode_teleport_flags(Data);
        recipe_display ->
            encode_recipe_display(Data);
        slot_display ->
            encode_slot_display(Data);
        {either_x_or_y, TypeX, TypeY} ->
            encode_either_x_or_y(Data, TypeX, TypeY);

	    %%        light_data ->
	    %%            encode_light_data(Data);
        game_profile ->
            encode_game_profile(Data);
        resolvable_profile ->
            encode_resolvable_profile(Data);
        {resolvable_profile, BodyOption, CapeOption, ElytraOption, ModelOption} ->
            encode_resolvable_profile(Data, BodyOption, CapeOption, ElytraOption, ModelOption);
        debug_subscription_event ->
            encode_debug_subscription_event(Data);
        debug_subscription_update ->
            encode_debug_subscription_update(Data);
        {debug_subscription_data, Type2} ->
            encode_debug_subscription_data(Data, Type2);
        debug_path_node ->
            encode_debug_path_node(Data);
        debug_structure_info ->
            encode_debug_structure_info(Data);
        debug_structure_piece ->
            encode_debug_structure_piece(Data);

        %% Component Data Types
        _ ->
            component_encode:encode_component(Type, Data)
    end.

encode_bool(#bool{bool = B}) -> encode_bool(B);
encode_bool(true) -> <<1:8>>;
encode_bool(false) -> <<0:8>>;
encode_bool(1) -> <<1:8>>;
encode_bool(0) -> <<0:8>>.

encode_byte(#byte{byte = Byte}) -> encode_byte(Byte);
encode_byte(Byte) when is_integer(Byte) ->
    <<Byte:8/signed-integer>>.

encode_ubyte(#ubyte{ubyte = UByte}) -> encode_ubyte(UByte);
encode_ubyte(UByte) when is_integer(UByte) ->
    <<UByte:8/unsigned-integer>>.

encode_short(#short{short = Short}) -> encode_short(Short);
encode_short(Short) when is_integer(Short) ->
    <<Short:16/signed-integer>>.

encode_ushort(#ushort{ushort = UShort}) -> encode_ushort(UShort);
encode_ushort(UShort) when is_integer(UShort) ->
    <<UShort:16/unsigned-integer>>.

encode_int(#int{int = Int}) -> encode_int(Int);
encode_int(Int) when is_integer(Int) ->
    <<Int:32/signed-integer>>.

encode_long(#long{long = Long}) -> encode_long(Long);
encode_long(Long) when is_integer(Long) ->
    <<Long:64/signed-integer>>.

encode_float(#float{float = Float}) -> encode_float(Float);
encode_float(Float) when is_number(Float) ->
    <<Float:32/float>>.

encode_double(#double{double = Double}) -> encode_double(Double);
encode_double(Double) when is_number(Double) ->
    <<Double:64/float>>.

encode_string(#string{string = String}) -> encode_string(String);
encode_string(String) when is_list(String) ->
    Bin = list_to_binary(String),
    LenBin = encode_varint(byte_size(Bin)),
    <<LenBin/binary, Bin/binary>>;
encode_string(String) when is_binary(String) ->
    LenBin = encode_varint(byte_size(String)),
    <<LenBin/binary, String/binary>>.

encode_varint(#varint{varint = I}) -> encode_varint(I);
encode_varint(I) when is_integer(I), I < 0 ->
    encode_varint(I band 16#FFFFFFFF);
encode_varint(I) when is_integer(I), I >= 0, I =< 127 ->
    <<I:8>>;
encode_varint(I) when is_integer(I), I > 127 ->
    <<1:1, (I band 127):7, (encode_varint(I bsr 7))/binary>>.

encode_varlong(#varlong{varlong = I}) -> encode_varlong(I);
encode_varlong(I) when is_integer(I), I < 0 ->
    encode_varlong(I band 16#FFFFFFFFFFFFFFFF);
encode_varlong(I) when is_integer(I), I >= 0, I =< 127 ->
    <<I:8>>;
encode_varlong(I) when is_integer(I), I > 127 ->
    <<1:1, (I band 127):7, (encode_varlong(I bsr 7))/binary>>.

encode_identifier(#identifier{identifier = String}) -> encode_identifier(String);
encode_identifier(String) ->
    encode_string(String).

encode_position(#position{x = X, y = Y, z = Z}) -> encode_position({X, Z, Y});
encode_position({X, Z, Y}) when is_integer(X), is_integer(Z), is_integer(Y) ->
    <<X:26/signed-integer, Z:26/signed-integer, Y:12/signed-integer>>.

encode_angle(#angle{angle = Angle}) -> encode_angle(Angle);
encode_angle(Angle) when is_integer(Angle) ->
    encode_byte(Angle);
encode_angle(Angle) when is_float(Angle) ->
    encode_byte(trunc(Angle)).

encode_uuid(#uuid{uuid = UUID}) -> encode_uuid(UUID);
encode_uuid(<<UUID:128/bitstring>>) ->
    <<UUID:128/bitstring>>;
encode_uuid(UUID) when is_integer(UUID) ->
    <<UUID:128/unsigned-integer>>;
encode_uuid(UUID) when is_list(UUID) ->
    list_to_binary(UUID).

encode_bitset(#bitset{bitset = BitSet}) -> encode_bitset(BitSet);
encode_bitset({Length, BitSet}) when is_integer(Length), is_integer(BitSet) ->
    LenBin = encode_varint(Length),
    <<LenBin/binary, BitSet:(Length*8)/signed-integer>>;
encode_bitset(BitSet) when is_integer(BitSet) ->
    Length = calc_bitset_bytes(BitSet),
    LenBin = encode_varint(Length),
    <<LenBin/binary, BitSet:(Length*8)/signed-integer>>.

encode_fixed_bitset(#fixed_bitset{fixed_bitset = BitSet}) -> encode_fixed_bitset(BitSet);
encode_fixed_bitset({Bits, BitSet}) when is_integer(Bits), is_integer(BitSet) ->
    LenBin = encode_varint(Bits),
    <<LenBin/binary, BitSet:Bits/signed-integer>>;
encode_fixed_bitset(BitSet) when is_integer(BitSet) ->
    Bits = calc_fixed_bitset_bits(BitSet),
    LenBin = encode_varint(Bits),
    <<LenBin/binary, BitSet:Bits/signed-integer>>.

encode_byte_array(#byte_array{byte_array = Data}) -> encode_byte_array(Data);
encode_byte_array(Data) when is_binary(Data) ->
    Data;
encode_byte_array(Data) when is_list(Data) ->
    list_to_binary(Data).

encode_byte_array(#byte_array{byte_array = Data}, Arg) -> encode_byte_array(Data, Arg);
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

encode_optional(#optional{some = some, optional = Value}, Type, true) ->
    encode_type(Value, Type);
encode_optional(#optional{some = none}, _Type, _Bool) ->
    <<>>;
encode_optional({some, Value}, Type, true) ->
    encode_type(Value, Type);
encode_optional(Value, Type, true) ->
    encode_type(Value, Type);
encode_optional(_Value, _Type, false) ->
    <<>>.

encode_prefixed_optional(#prefixed_optional{some = some, prefixed_optional = Value}, InnerType) ->
    encode_prefixed_optional({some, Value}, InnerType);
encode_prefixed_optional(#prefixed_optional{some = none}, InnerType) ->
    encode_prefixed_optional(none, InnerType);
encode_prefixed_optional({some, Value}, InnerType) ->
    <<(encode_bool(true))/binary, (encode_type(Value, InnerType))/binary>>;
encode_prefixed_optional(none, _InnerType) ->
    encode_bool(false);
encode_prefixed_optional(undefined, _InnerType) ->
    encode_bool(false);
encode_prefixed_optional(Value, InnerType) ->
    <<(encode_bool(true))/binary, (encode_type(Value, InnerType))/binary>>.

encode_id_or_x(Data) ->
    encode_id_or_x(Data, varint).

encode_id_or_x(#id_or_x{id_or_x = Value}, InnerType) ->
    encode_id_or_x(Value, InnerType);
encode_id_or_x({id, Id}, _InnerType) when is_integer(Id), Id >= 0 ->
    encode_varint(Id + 1);
encode_id_or_x({val, Value}, InnerType) ->
    IDBin = encode_varint(0),
    ValBin = encode_type(Value, InnerType),
    <<IDBin/binary, ValBin/binary>>;
encode_id_or_x({value, Value}, InnerType) ->
    IDBin = encode_varint(0),
    ValBin = encode_type(Value, InnerType),
    <<IDBin/binary, ValBin/binary>>;
encode_id_or_x({inline, Value}, InnerType) ->
    IDBin = encode_varint(0),
    ValBin = encode_type(Value, InnerType),
    <<IDBin/binary, ValBin/binary>>;
encode_id_or_x(Value, InnerType) ->
    IDBin = encode_varint(0),
    ValBin = encode_type(Value, InnerType),
    <<IDBin/binary, ValBin/binary>>.

encode_array(#array{array = List}, ElemType) ->
    encode_array(List, ElemType);
encode_array(List, ElemType) when is_list(List) ->
    encode_array_loop(List, ElemType, <<>>);
encode_array(List, {Count, ElemType}) when is_list(List), is_integer(Count) ->
    encode_array_loop(lists:sublist(List, Count), ElemType, <<>>);
encode_array(List, {ElemType, Count}) when is_list(List), is_integer(Count) ->
    encode_array_loop(lists:sublist(List, Count), ElemType, <<>>).

encode_array(#array{array = List}, Arg1, Arg2) ->
    encode_array(List, Arg1, Arg2);
encode_array(List, Count, ElemType) when is_list(List), is_integer(Count) ->
    encode_array_loop(lists:sublist(List, Count), ElemType, <<>>);
encode_array(List, ElemType, Count) when is_list(List), is_integer(Count) ->
    encode_array_loop(lists:sublist(List, Count), ElemType, <<>>).

encode_array_loop([], _ElemType, Acc) ->
    Acc;
encode_array_loop([Head | Tail], ElemType, Acc) ->
    ElemBin = encode_type(Head, ElemType),
    encode_array_loop(Tail, ElemType, <<Acc/binary, ElemBin/binary>>).

encode_prefixed_array(#prefixed_array{prefixed_array = List}, ElemType) ->
    encode_prefixed_array(List, ElemType);
encode_prefixed_array(List, ElemType) when is_list(List) ->
    LenBin = encode_varint(length(List)),
    ArrayBin = encode_array_loop(List, ElemType, <<>>),
    <<LenBin/binary, ArrayBin/binary>>.

encode_prefixed_array(#prefixed_array{prefixed_array = List}, PrefixType, ElemType) ->
    encode_prefixed_array(List, PrefixType, ElemType);
encode_prefixed_array(List, PrefixType, ElemType) when is_list(List) ->
    LenBin = encode_type(length(List), PrefixType),
    ArrayBin = encode_array_loop(List, ElemType, <<>>),
    <<LenBin/binary, ArrayBin/binary>>.

encode_prefixed_array_list(#prefixed_array{prefixed_array = List}, ElemList) ->
    encode_prefixed_array_list(List, ElemList);
encode_prefixed_array_list(Data, ElemList) ->
    encode_prefixed_array_list(Data, varint, ElemList).

encode_prefixed_array_list(#prefixed_array{prefixed_array = List}, PrefixType, ElemList) ->
    encode_prefixed_array_list(List, PrefixType, ElemList);
encode_prefixed_array_list(List, PrefixType, ElemList) when is_list(List), is_list(ElemList) ->
    LenBin = encode_type(length(List), PrefixType),
    ItemsBin = list_to_binary([encode_tuple_elements(Item, ElemList) || Item <- List]),
    <<LenBin/binary, ItemsBin/binary>>.

encode_tuple_elements(Tuple, ElemList) when is_tuple(Tuple) ->
    encode_tuple_elements(tuple_to_list(Tuple), ElemList);
encode_tuple_elements(List, ElemList) when is_list(List) ->
    list_to_binary(lists:zipwith(fun(Val, Type) -> encode_type(Val, Type) end, List, ElemList)).

encode_enum(#enum{enum = Val}) ->
    encode_enum(Val);
encode_enum(Data) ->
    encode_enum(Data, varint).

encode_enum(#enum{enum = Val}, Arg1) ->
    encode_enum(Val, Arg1);
encode_enum(Data, InnerType) when is_atom(InnerType) ->
    encode_type(Data, InnerType);
encode_enum(Data, EnumList) when is_list(EnumList); is_map(EnumList) ->
    encode_enum(Data, varint, EnumList).

encode_enum(#enum{enum = Val}, Arg1, Arg2) ->
    encode_enum(Val, Arg1, Arg2);
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
encode_slot(#slot{item_count = 0}) ->
    encode_varint(0);
encode_slot(#slot{item_count = ItemCount, itemID = ItemID, components_to_add = ComponentsToAdd, components_to_remove = ComponentsToRemove}) ->
    encode_slot({ItemCount, ItemID, ComponentsToAdd, ComponentsToRemove});
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
encode_hashed_slot(#hashed_slot{item_count = 0}) ->
    encode_bool(false);
encode_hashed_slot(#hashed_slot{item_count = ItemCount, itemID = ItemID, components_to_add = ComponentsToAdd, components_to_remove = ComponentsToRemove}) ->
    encode_hashed_slot({ItemID, ItemCount, ComponentsToAdd, ComponentsToRemove});
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

encode_text_component(Data) ->
    SNBTBin = text_component:to_snbt(Data),
    encode_string(SNBTBin).

encode_json_text_component(Data) when is_map(Data) ->
    JsonBin = iolist_to_binary(json:encode(Data)),
    encode_string(JsonBin);
encode_json_text_component(Data) when is_binary(Data) ->
    encode_string(Data);
encode_json_text_component(Data) when is_list(Data) ->
    case io_lib:printable_unicode_list(Data) of
        true ->
            encode_string(Data);
        false ->
            JsonBin = iolist_to_binary(json:encode(Data)),
            encode_string(JsonBin)
    end;
encode_json_text_component(Data) ->
    JsonBin = iolist_to_binary(json:encode(Data)),
    encode_string(JsonBin).

encode_id_set(#id_set{id_set = Data}) ->
    encode_id_set(Data);
encode_id_set(Data) ->
    case Data of
        {tag, TagName} ->
            TypeBin = encode_varint(0),
            TagBin  = encode_string(TagName),
            <<TypeBin/binary, TagBin/binary>>;
        {ids, IDs} when is_list(IDs) ->
            TypeBin = encode_varint(length(IDs) + 1),
            IDsBin  = encode_array(IDs, varint),
            <<TypeBin/binary, IDsBin/binary>>;
        TagName when is_binary(TagName) ->
            TypeBin = encode_varint(0),
            TagBin  = encode_string(TagName),
            <<TypeBin/binary, TagBin/binary>>;
        IDs when is_list(IDs) ->
            case is_tag_name(IDs) of
                true ->
                    TypeBin = encode_varint(0),
                    TagBin  = encode_string(IDs),
                    <<TypeBin/binary, TagBin/binary>>;
                false ->
                    TypeBin = encode_varint(length(IDs) + 1),
                    IDsBin  = encode_array(IDs, varint),
                    <<TypeBin/binary, IDsBin/binary>>
            end
    end.

is_tag_name(List) ->
    lists:any(fun(C) -> (C >= $a andalso C =< $z) orelse C == $: orelse C == $/ end, List).

encode_sound_event(#sound_event{sound_name = SoundName, has_fixed_value = true, fixed_range = FixedRange}) ->
    encode_sound_event({SoundName, true, FixedRange});
encode_sound_event(#sound_event{sound_name = SoundName, has_fixed_value = false, fixed_range = FixedRange}) ->
    encode_sound_event({SoundName, false, FixedRange});
encode_sound_event({SoundName, true, FixedRange}) ->
    SoundBin = encode_string(SoundName),
    BoolBin = encode_bool(true),
    FloatBin = encode_float(FixedRange),
    <<SoundBin/binary, BoolBin/binary, FloatBin/binary>>;
encode_sound_event({SoundName, false, _FixedRange}) ->
    SoundBin = encode_string(SoundName),
    BoolBin = encode_bool(false),
    <<SoundBin/binary, BoolBin/binary>>;
encode_sound_event({SoundName, false}) ->
    SoundBin = encode_string(SoundName),
    BoolBin = encode_bool(false),
    <<SoundBin/binary, BoolBin/binary>>.

encode_teleport_flags(#teleport_flags{flagsmap = FlagsMap}) ->
    encode_teleport_flags(FlagsMap);
encode_teleport_flags(Int) when is_integer(Int) ->
    encode_int(Int);
encode_teleport_flags(Map) when is_map(Map) ->
    BitX = case maps:get(relative_x, Map, false) of true -> 16#0001; false -> 0 end,
    BitY = case maps:get(relative_y, Map, false) of true -> 16#0002; false -> 0 end,
    BitZ = case maps:get(relative_z, Map, false) of true -> 16#0004; false -> 0 end,
    BitYaw = case maps:get(relative_yaw, Map, false) of true -> 16#0008; false -> 0 end,
    BitPitch = case maps:get(relative_pitch, Map, false) of true -> 16#0010; false -> 0 end,
    BitVelX = case maps:get(relative_velocity_x, Map, false) of true -> 16#0020; false -> 0 end,
    BitVelY = case maps:get(relative_velocity_y, Map, false) of true -> 16#0040; false -> 0 end,
    BitVelZ = case maps:get(relative_velocity_z, Map, false) of true -> 16#0080; false -> 0 end,
    BitRot = case maps:get(rotate_velocity, Map, false) of true -> 16#0100; false -> 0 end,
    Int = BitX bor BitY bor BitZ bor BitYaw bor BitPitch bor BitVelX bor BitVelY bor BitVelZ bor BitRot,
    encode_int(Int);
encode_teleport_flags(FlagsList) when is_list(FlagsList) ->
    Int = lists:foldl(fun(Flag, Acc) ->
        Mask = case Flag of
            relative_x -> 16#0001;
            relative_y -> 16#0002;
            relative_z -> 16#0004;
            relative_yaw -> 16#0008;
            relative_pitch -> 16#0010;
            relative_velocity_x -> 16#0020;
            relative_velocity_y -> 16#0040;
            relative_velocity_z -> 16#0080;
            rotate_velocity -> 16#0100;
            _ -> 0
        end,
        Acc bor Mask
    end, 0, FlagsList),
    encode_int(Int).

encode_slot_display(#empty{}) ->
    encode_varint(0);
encode_slot_display(#any_fuel{}) ->
    encode_varint(1);
encode_slot_display(#with_any_potion{base = Base}) ->
    TypeBin = encode_varint(2),
    BaseBin = encode_slot_display(Base),
    <<TypeBin/binary, BaseBin/binary>>;
encode_slot_display(#only_with_component{base = Base, component_type_id = ComponentTypeID}) ->
    TypeBin = encode_varint(3),
    BaseBin = encode_slot_display(Base),
    CompBin = encode_varint(ComponentTypeID),
    <<TypeBin/binary, BaseBin/binary, CompBin/binary>>;
encode_slot_display(#item{item_type = ItemType}) ->
    TypeBin = encode_varint(4),
    ItemBin = encode_varint(ItemType),
    <<TypeBin/binary, ItemBin/binary>>;
encode_slot_display(#item_stack{item_stack = ItemStack}) ->
    TypeBin = encode_varint(5),
    StackBin = encode_slot(ItemStack),
    <<TypeBin/binary, StackBin/binary>>;
encode_slot_display(#tag{tag = Tag}) ->
    TypeBin = encode_varint(6),
    TagBin = encode_string(Tag),
    <<TypeBin/binary, TagBin/binary>>;
encode_slot_display(#dyed{dye = Dye, target = Target}) ->
    TypeBin = encode_varint(7),
    DyeBin = encode_slot_display(Dye),
    TargetBin = encode_slot_display(Target),
    <<TypeBin/binary, DyeBin/binary, TargetBin/binary>>;
encode_slot_display(#smithing_trim{base = Base, material = Material, pattern = Pattern}) ->
    TypeBin = encode_varint(8),
    BaseBin = encode_slot_display(Base),
    MatBin = encode_slot_display(Material),
    PatBin = encode_varint(Pattern),
    <<TypeBin/binary, BaseBin/binary, MatBin/binary, PatBin/binary>>;
encode_slot_display(#with_remainder{ingredient = Ingredient, remainder = Remainder}) ->
    TypeBin = encode_varint(9),
    IngBin = encode_slot_display(Ingredient),
    RemBin = encode_slot_display(Remainder),
    <<TypeBin/binary, IngBin/binary, RemBin/binary>>;
encode_slot_display(#composite{options = Options}) ->
    TypeBin = encode_varint(10),
    OptionsBin = encode_prefixed_array(Options, slot_display),
    <<TypeBin/binary, OptionsBin/binary>>.


encode_recipe_display(#crafting_shapeless{ingredients_count = Count, ingredients = Ingredients, result = Result, crafting_station = CraftingStation}) ->
    TypeBin = encode_varint(0),
    ActualCount = case Count of
        undefined -> length(Ingredients);
        _ -> Count
    end,
    CountBin = encode_varint(ActualCount),
    IngsBin = encode_array(Ingredients, slot_display),
    ResBin = encode_slot_display(Result),
    StationBin = encode_slot_display(CraftingStation),
    <<TypeBin/binary, CountBin/binary, IngsBin/binary, ResBin/binary, StationBin/binary>>;
encode_recipe_display(#crafting_shaped{width = Width, height = Height, ingredients_count = Count, ingredients = Ingredients, result = Result, crafting_station = CraftingStation}) ->
    TypeBin = encode_varint(1),
    WidthBin = encode_varint(Width),
    HeightBin = encode_varint(Height),
    ActualCount = case Count of
        undefined -> length(Ingredients);
        _ -> Count
    end,
    CountBin = encode_varint(ActualCount),
    IngsBin = encode_array(Ingredients, slot_display),
    ResBin = encode_slot_display(Result),
    StationBin = encode_slot_display(CraftingStation),
    <<TypeBin/binary, WidthBin/binary, HeightBin/binary, CountBin/binary, IngsBin/binary, ResBin/binary, StationBin/binary>>;
encode_recipe_display(#furnace{ingredient = Ingredient, fuel = Fuel, result = Result, crafting_station = CraftingStation, cooking_time = CookingTime, experience = Experience}) ->
    TypeBin = encode_varint(2),
    IngBin = encode_slot_display(Ingredient),
    FuelBin = encode_slot_display(Fuel),
    ResBin = encode_slot_display(Result),
    StationBin = encode_slot_display(CraftingStation),
    CookBin = encode_varint(CookingTime),
    ExpBin = encode_float(Experience),
    <<TypeBin/binary, IngBin/binary, FuelBin/binary, ResBin/binary, StationBin/binary, CookBin/binary, ExpBin/binary>>;
encode_recipe_display(#stonecutter{ingredient = Ingredient, result = Result, crafting_station = CraftingStation}) ->
    TypeBin = encode_varint(3),
    IngBin = encode_slot_display(Ingredient),
    ResBin = encode_slot_display(Result),
    StationBin = encode_slot_display(CraftingStation),
    <<TypeBin/binary, IngBin/binary, ResBin/binary, StationBin/binary>>;
encode_recipe_display(#smithing{template = Template, base = Base, addition = Addition, result = Result, crafting_station = CraftingStation}) ->
    TypeBin = encode_varint(4),
    TmplBin = encode_slot_display(Template),
    BaseBin = encode_slot_display(Base),
    AddBin = encode_slot_display(Addition),
    ResBin = encode_slot_display(Result),
    StationBin = encode_slot_display(CraftingStation),
    <<TypeBin/binary, TmplBin/binary, BaseBin/binary, AddBin/binary, ResBin/binary, StationBin/binary>>.

encode_either_x_or_y({left, Value}, TypeX, _TypeY) ->
    <<(encode_bool(true))/binary, (encode_type(Value, TypeX))/binary>>;
encode_either_x_or_y({x, Value}, TypeX, _TypeY) ->
    <<(encode_bool(true))/binary, (encode_type(Value, TypeX))/binary>>;
encode_either_x_or_y({true, Value}, TypeX, _TypeY) ->
    <<(encode_bool(true))/binary, (encode_type(Value, TypeX))/binary>>;
encode_either_x_or_y({right, Value}, _TypeX, TypeY) ->
    <<(encode_bool(false))/binary, (encode_type(Value, TypeY))/binary>>;
encode_either_x_or_y({y, Value}, _TypeX, TypeY) ->
    <<(encode_bool(false))/binary, (encode_type(Value, TypeY))/binary>>;
encode_either_x_or_y({false, Value}, _TypeX, TypeY) ->
    <<(encode_bool(false))/binary, (encode_type(Value, TypeY))/binary>>.

encode_game_profile(#game_profile{uuid = UUID, username = Username, properties = Properties}) ->
    encode_game_profile({UUID, Username, Properties});
encode_game_profile({UUID, Username, Properties}) ->
    UUIDBin = encode_uuid(UUID),
    UsernameBin = encode_string(Username),
    CountBin = encode_varint(length(Properties)),
    PropertiesBin = encode_properties(Properties),
    <<UUIDBin/binary, UsernameBin/binary, CountBin/binary, PropertiesBin/binary>>.

encode_properties(Properties) when is_list(Properties) ->
    iolist_to_binary([encode_property(Prop) || Prop <- Properties]).

encode_property({Name, Value, Signature}) ->
    NameBin = encode_string(Name),
    ValueBin = encode_string(Value),
    SigBin = encode_prefixed_optional(Signature, string),
    <<NameBin/binary, ValueBin/binary, SigBin/binary>>;
encode_property({Name, Value}) ->
    encode_property({Name, Value, none});
encode_property(#{name := Name, value := Value} = Prop) ->
    Sig = maps:get(signature, Prop, none),
    encode_property({Name, Value, Sig}).

encode_resolvable_profile({ProfileKind, Profile, Body, Cape, Elytra, Model}, BodyOption, CapeOption, ElytraOption, ModelOption) ->
    ProfileKindBin = encode_varint(ProfileKind),
    ProfileBin = pack_resolvable_profile(ProfileKind, Profile),
    BodyBin = encode_optional(Body, identifier, BodyOption),
    CapeBin = encode_optional(Cape, identifier, CapeOption),
    ElytraBin = encode_optional(Elytra, identifier, ElytraOption),
    ModelBin = encode_optional(Model, varint, ModelOption),
    <<ProfileKindBin/binary, ProfileBin/binary, BodyBin/binary, CapeBin/binary, ElytraBin/binary, ModelBin/binary>>;
encode_resolvable_profile(Data, BodyOption, CapeOption, ElytraOption, ModelOption) when is_map(Data) ->
    ProfileKind = maps:get(profile_kind, Data, 0),
    Profile = maps:get(profile, Data, {}),
    Body = maps:get(body, Data, none),
    Cape = maps:get(cape, Data, none),
    Elytra = maps:get(elytra, Data, none),
    Model = maps:get(model, Data, none),
    encode_resolvable_profile({ProfileKind, Profile, Body, Cape, Elytra, Model}, BodyOption, CapeOption, ElytraOption, ModelOption).

encode_resolvable_profile({ProfileKind, Profile, Body, Cape, Elytra, Model}) ->
    BodyOption = Body =/= none andalso Body =/= undefined,
    CapeOption = Cape =/= none andalso Cape =/= undefined,
    ElytraOption = Elytra =/= none andalso Elytra =/= undefined,
    ModelOption = Model =/= none andalso Model =/= undefined,
    encode_resolvable_profile({ProfileKind, Profile, Body, Cape, Elytra, Model}, BodyOption, CapeOption, ElytraOption, ModelOption);
encode_resolvable_profile(#{profile_kind := ProfileKind} = Map) ->
    Profile = maps:get(profile, Map, {}),
    Body = maps:get(body, Map, none),
    Cape = maps:get(cape, Map, none),
    Elytra = maps:get(elytra, Map, none),
    Model = maps:get(model, Map, none),
    encode_resolvable_profile({ProfileKind, Profile, Body, Cape, Elytra, Model}).

pack_resolvable_profile(0, {Username, UUID, Properties}) ->
    UserBin = encode_prefixed_optional(Username, string),
    UuidBin = encode_prefixed_optional(UUID, uuid),
    LenBin = encode_varint(length(Properties)),
    PropsBin = encode_properties(Properties),
    <<UserBin/binary, UuidBin/binary, LenBin/binary, PropsBin/binary>>;
pack_resolvable_profile(1, GameProfile) ->
    encode_game_profile(GameProfile).

encode_debug_subscription_event(#debug_subscription_event{debug_subscription_type = Type, data = Data}) when is_integer(Type) ->
    TypeBin = encode_enum(Type),
    DataBin = encode_debug_subscription_data(Data, Type),
    <<TypeBin/binary, DataBin/binary>>;
encode_debug_subscription_event(#debug_subscription_event{data = Data}) ->
    Type = debug_subscription_type_id(Data),
    TypeBin = encode_enum(Type),
    DataBin = encode_debug_subscription_data(Data, Type),
    <<TypeBin/binary, DataBin/binary>>;
encode_debug_subscription_event({Type, Data}) when is_integer(Type) ->
    TypeBin = encode_enum(Type),
    DataBin = encode_debug_subscription_data(Data, Type),
    <<TypeBin/binary, DataBin/binary>>;
encode_debug_subscription_event(Record) ->
    Type = debug_subscription_type_id(Record),
    TypeBin = encode_enum(Type),
    DataBin = encode_debug_subscription_data(Record, Type),
    <<TypeBin/binary, DataBin/binary>>.

encode_debug_subscription_update({Type, OptData}) when is_integer(Type) ->
    TypeBin = encode_enum(Type),
    DataBin = encode_prefixed_optional(OptData, {debug_subscription_data, Type}),
    <<TypeBin/binary, DataBin/binary>>;
encode_debug_subscription_update({some, Record}) ->
    Type = debug_subscription_type_id(Record),
    TypeBin = encode_enum(Type),
    DataBin = encode_prefixed_optional({some, Record}, {debug_subscription_data, Type}),
    <<TypeBin/binary, DataBin/binary>>;
encode_debug_subscription_update(none) ->
    TypeBin = encode_enum(0),
    DataBin = encode_prefixed_optional(none, {debug_subscription_data, 0}),
    <<TypeBin/binary, DataBin/binary>>;
encode_debug_subscription_update(undefined) ->
    TypeBin = encode_enum(0),
    DataBin = encode_prefixed_optional(none, {debug_subscription_data, 0}),
    <<TypeBin/binary, DataBin/binary>>;
encode_debug_subscription_update(Record) when is_tuple(Record) ->
    Type = debug_subscription_type_id(Record),
    TypeBin = encode_enum(Type),
    DataBin = encode_prefixed_optional({some, Record}, {debug_subscription_data, Type}),
    <<TypeBin/binary, DataBin/binary>>.

encode_debug_subscription_data(_Data, 0) ->
    <<>>;
encode_debug_subscription_data(#bee{hive_position = HivePos, flower_position = FlowerPos, travel_ticks = Ticks, blacklisted_hives = Blacklist}, 1) ->
    HiveBin = encode_prefixed_optional(HivePos, position),
    FlowerBin = encode_prefixed_optional(FlowerPos, position),
    TicksBin = encode_varint(Ticks),
    BlacklistBin = encode_prefixed_array(Blacklist, position),
    <<HiveBin/binary, FlowerBin/binary, TicksBin/binary, BlacklistBin/binary>>;
encode_debug_subscription_data({HivePos, FlowerPos, Ticks, Blacklist}, 1) ->
    HiveBin = encode_prefixed_optional(HivePos, position),
    FlowerBin = encode_prefixed_optional(FlowerPos, position),
    TicksBin = encode_varint(Ticks),
    BlacklistBin = encode_prefixed_array(Blacklist, position),
    <<HiveBin/binary, FlowerBin/binary, TicksBin/binary, BlacklistBin/binary>>;
encode_debug_subscription_data(#villager_brain{name = Name, profession = Prof, xp = XP, health = Health, max_health = MaxHealth, inventory = Inv, wants_golem = WantsGolem, anger_level = Anger, activities = Act, behaviors = Beh, memories = Mem, gossips = Gos, pois = POIs, potential_pois = PotPOIs}, 2) ->
    NameBin = encode_string(Name),
    ProfBin = encode_string(Prof),
    XpBin = encode_int(XP),
    HealthBin = encode_float(Health),
    MaxHealthBin = encode_float(MaxHealth),
    InvBin = encode_string(Inv),
    WantsGolemBin = encode_bool(WantsGolem),
    AngerBin = encode_int(Anger),
    ActBin = encode_prefixed_array(Act, string),
    BehBin = encode_prefixed_array(Beh, string),
    MemBin = encode_prefixed_array(Mem, string),
    GosBin = encode_prefixed_array(Gos, string),
    PoisBin = encode_prefixed_array(POIs, position),
    PotPoisBin = encode_prefixed_array(PotPOIs, position),
    <<NameBin/binary, ProfBin/binary, XpBin/binary, HealthBin/binary, MaxHealthBin/binary, InvBin/binary, WantsGolemBin/binary, AngerBin/binary, ActBin/binary, BehBin/binary, MemBin/binary, GosBin/binary, PoisBin/binary, PotPoisBin/binary>>;
encode_debug_subscription_data(#breeze{attack_target = Atk, jump_target = Jump}, 3) ->
    AtkBin = encode_prefixed_optional(Atk, varint),
    JumpBin = encode_prefixed_optional(Jump, position),
    <<AtkBin/binary, JumpBin/binary>>;
encode_debug_subscription_data({Atk, Jump}, 3) ->
    AtkBin = encode_prefixed_optional(Atk, varint),
    JumpBin = encode_prefixed_optional(Jump, position),
    <<AtkBin/binary, JumpBin/binary>>;
encode_debug_subscription_data(#goal_selector{priority = Priority, is_running = IsRunning, name = Name}, 4) ->
    PrioBin = encode_varint(Priority),
    RunBin = encode_bool(IsRunning),
    NameBin = encode_string(Name),
    <<PrioBin/binary, RunBin/binary, NameBin/binary>>;
encode_debug_subscription_data({Priority, IsRunning, Name}, 4) ->
    PrioBin = encode_varint(Priority),
    RunBin = encode_bool(IsRunning),
    NameBin = encode_string(Name),
    <<PrioBin/binary, RunBin/binary, NameBin/binary>>;
encode_debug_subscription_data(#entity_path{reached = Reached, next_block_index = NextIdx, block_position = Pos, nodes = Nodes, target_nodes = TargetNodes, open_set = OpenSet, closed_set = ClosedSet, max_node_distance = MaxDist}, 5) ->
    ReachedBin = encode_bool(Reached),
    NextIdxBin = encode_int(NextIdx),
    PosBin = encode_position(Pos),
    NodesBin = encode_prefixed_array(Nodes, debug_path_node),
    TargetNodesBin = encode_prefixed_array(TargetNodes, debug_path_node),
    OpenSetBin = encode_prefixed_array(OpenSet, debug_path_node),
    ClosedSetBin = encode_prefixed_array(ClosedSet, debug_path_node),
    MaxDistBin = encode_float(MaxDist),
    <<ReachedBin/binary, NextIdxBin/binary, PosBin/binary, NodesBin/binary, TargetNodesBin/binary, OpenSetBin/binary, ClosedSetBin/binary, MaxDistBin/binary>>;
encode_debug_subscription_data({Reached, NextIdx, Pos, Nodes, TargetNodes, OpenSet, ClosedSet, MaxDist}, 5) ->
    ReachedBin = encode_bool(Reached),
    NextIdxBin = encode_int(NextIdx),
    PosBin = encode_position(Pos),
    NodesBin = encode_prefixed_array(Nodes, debug_path_node),
    TargetNodesBin = encode_prefixed_array(TargetNodes, debug_path_node),
    OpenSetBin = encode_prefixed_array(OpenSet, debug_path_node),
    ClosedSetBin = encode_prefixed_array(ClosedSet, debug_path_node),
    MaxDistBin = encode_float(MaxDist),
    <<ReachedBin/binary, NextIdxBin/binary, PosBin/binary, NodesBin/binary, TargetNodesBin/binary, OpenSetBin/binary, ClosedSetBin/binary, MaxDistBin/binary>>;
encode_debug_subscription_data(#entity_block_intersection{id = ID}, 6) ->
    encode_enum(ID);
encode_debug_subscription_data(ID, 6) when is_integer(ID) ->
    encode_enum(ID);
encode_debug_subscription_data(#bee_hive{hive_type = HiveType, occupant_count = OccCount, honey_level = HoneyLevel, sedated = Sedated}, 7) ->
    TypeBin = encode_enum(HiveType),
    OccBin = encode_varint(OccCount),
    HoneyBin = encode_varint(HoneyLevel),
    SedatedBin = encode_bool(Sedated),
    <<TypeBin/binary, OccBin/binary, HoneyBin/binary, SedatedBin/binary>>;
encode_debug_subscription_data({HiveType, OccCount, HoneyLevel, Sedated}, 7) ->
    TypeBin = encode_enum(HiveType),
    OccBin = encode_varint(OccCount),
    HoneyBin = encode_varint(HoneyLevel),
    SedatedBin = encode_bool(Sedated),
    <<TypeBin/binary, OccBin/binary, HoneyBin/binary, SedatedBin/binary>>;
encode_debug_subscription_data(#poi{position = Pos, poi_type = POIType, free_ticket_count = Count}, 8) ->
    PosBin = encode_position(Pos),
    TypeBin = encode_enum(POIType),
    CountBin = encode_varint(Count),
    <<PosBin/binary, TypeBin/binary, CountBin/binary>>;
encode_debug_subscription_data({Pos, POIType, Count}, 8) ->
    PosBin = encode_position(Pos),
    TypeBin = encode_enum(POIType),
    CountBin = encode_varint(Count),
    <<PosBin/binary, TypeBin/binary, CountBin/binary>>;
encode_debug_subscription_data(#redstone_wire_orientation{id = ID}, 9) ->
    encode_varint(ID);
encode_debug_subscription_data(ID, 9) when is_integer(ID) ->
    encode_varint(ID);
encode_debug_subscription_data(_Data, 10) ->
    <<>>;
encode_debug_subscription_data(#raid{positions = Positions}, 11) ->
    encode_prefixed_array(Positions, position);
encode_debug_subscription_data(Positions, 11) when is_list(Positions) ->
    encode_prefixed_array(Positions, position);
encode_debug_subscription_data(#structure{structures = Structures}, 12) ->
    encode_prefixed_array(Structures, debug_structure_info);
encode_debug_subscription_data(Structures, 12) when is_list(Structures) ->
    encode_prefixed_array(Structures, debug_structure_info);
encode_debug_subscription_data(#game_event_listener{listener_radius = Radius}, 13) ->
    encode_varint(Radius);
encode_debug_subscription_data(Radius, 13) when is_integer(Radius) ->
    encode_varint(Radius);
encode_debug_subscription_data(#neighbor_update{position = Pos}, 14) ->
    encode_position(Pos);
encode_debug_subscription_data(Pos, 14) when is_tuple(Pos) ->
    encode_position(Pos);
encode_debug_subscription_data(#game_event{event = Event, x = X, y = Y, z = Z}, 15) ->
    EventBin = encode_enum(Event),
    XBin = encode_double(X),
    YBin = encode_double(Y),
    ZBin = encode_double(Z),
    <<EventBin/binary, XBin/binary, YBin/binary, ZBin/binary>>;
encode_debug_subscription_data({Event, X, Y, Z}, 15) ->
    EventBin = encode_enum(Event),
    XBin = encode_double(X),
    YBin = encode_double(Y),
    ZBin = encode_double(Z),
    <<EventBin/binary, XBin/binary, YBin/binary, ZBin/binary>>.

encode_debug_path_node(#debug_path_node{x = X, y = Y, z = Z, walk_cost = WalkCost, penalty = Penalty, open = Open, type = Type, heap_index = HeapIndex}) ->
    encode_debug_path_node({X, Y, Z, WalkCost, Penalty, Open, Type, HeapIndex});
encode_debug_path_node({X, Y, Z, WalkCost, Penalty, Open, Type, HeapIndex}) ->
    XBin = encode_int(X),
    YBin = encode_int(Y),
    ZBin = encode_int(Z),
    WalkBin = encode_float(WalkCost),
    PenBin = encode_float(Penalty),
    OpenBin = encode_bool(Open),
    TypeBin = encode_varint(Type),
    HeapBin = encode_int(HeapIndex),
    <<XBin/binary, YBin/binary, ZBin/binary, WalkBin/binary, PenBin/binary, OpenBin/binary, TypeBin/binary, HeapBin/binary>>.

encode_debug_structure_info(#debug_structure_info{min_x = MinX, min_y = MinY, min_z = MinZ, max_x = MaxX, max_y = MaxY, max_z = MaxZ, pieces = Pieces}) ->
    encode_debug_structure_info({{MinX, MinY, MinZ, MaxX, MaxY, MaxZ}, Pieces});
encode_debug_structure_info({{MinX, MinY, MinZ, MaxX, MaxY, MaxZ}, Pieces}) ->
    MinXBin = encode_int(MinX),
    MinYBin = encode_int(MinY),
    MinZBin = encode_int(MinZ),
    MaxXBin = encode_int(MaxX),
    MaxYBin = encode_int(MaxY),
    MaxZBin = encode_int(MaxZ),
    PiecesBin = encode_prefixed_array(Pieces, debug_structure_piece),
    <<MinXBin/binary, MinYBin/binary, MinZBin/binary, MaxXBin/binary, MaxYBin/binary, MaxZBin/binary, PiecesBin/binary>>.

encode_debug_structure_piece(#debug_structure_piece{min_x = MinX, min_y = MinY, min_z = MinZ, max_x = MaxX, max_y = MaxY, max_z = MaxZ, is_start = IsStart}) ->
    encode_debug_structure_piece({{MinX, MinY, MinZ, MaxX, MaxY, MaxZ}, IsStart});
encode_debug_structure_piece({{MinX, MinY, MinZ, MaxX, MaxY, MaxZ}, IsStart}) ->
    MinXBin = encode_int(MinX),
    MinYBin = encode_int(MinY),
    MinZBin = encode_int(MinZ),
    MaxXBin = encode_int(MaxX),
    MaxYBin = encode_int(MaxY),
    MaxZBin = encode_int(MaxZ),
    StartBin = encode_bool(IsStart),
    <<MinXBin/binary, MinYBin/binary, MinZBin/binary, MaxXBin/binary, MaxYBin/binary, MaxZBin/binary, StartBin/binary>>.

debug_subscription_type_id(0) -> 0;
debug_subscription_type_id(1) -> 1;
debug_subscription_type_id(2) -> 2;
debug_subscription_type_id(3) -> 3;
debug_subscription_type_id(4) -> 4;
debug_subscription_type_id(5) -> 5;
debug_subscription_type_id(6) -> 6;
debug_subscription_type_id(7) -> 7;
debug_subscription_type_id(8) -> 8;
debug_subscription_type_id(9) -> 9;
debug_subscription_type_id(10) -> 10;
debug_subscription_type_id(11) -> 11;
debug_subscription_type_id(12) -> 12;
debug_subscription_type_id(13) -> 13;
debug_subscription_type_id(14) -> 14;
debug_subscription_type_id(15) -> 15;
debug_subscription_type_id(#debug_subscription_event{debug_subscription_type = Type}) when is_integer(Type) -> Type;
debug_subscription_type_id(#debug_subscription_event{data = Data}) -> debug_subscription_type_id(Data);
debug_subscription_type_id(#dedicated_server_tick_time{}) -> 0;
debug_subscription_type_id(#bee{}) -> 1;
debug_subscription_type_id(#villager_brain{}) -> 2;
debug_subscription_type_id(#breeze{}) -> 3;
debug_subscription_type_id(#goal_selector{}) -> 4;
debug_subscription_type_id(#entity_path{}) -> 5;
debug_subscription_type_id(#entity_block_intersection{}) -> 6;
debug_subscription_type_id(#bee_hive{}) -> 7;
debug_subscription_type_id(#poi{}) -> 8;
debug_subscription_type_id(#redstone_wire_orientation{}) -> 9;
debug_subscription_type_id(#village_section{}) -> 10;
debug_subscription_type_id(#raid{}) -> 11;
debug_subscription_type_id(#structure{}) -> 12;
debug_subscription_type_id(#game_event_listener{}) -> 13;
debug_subscription_type_id(#neighbor_update{}) -> 14;
debug_subscription_type_id(#game_event{}) -> 15;
debug_subscription_type_id(dedicated_server_tick_time) -> 0;
debug_subscription_type_id(bee) -> 1;
debug_subscription_type_id(villager_brain) -> 2;
debug_subscription_type_id(breeze) -> 3;
debug_subscription_type_id(goal_selector) -> 4;
debug_subscription_type_id(entity_path) -> 5;
debug_subscription_type_id(entity_block_intersection) -> 6;
debug_subscription_type_id(bee_hive) -> 7;
debug_subscription_type_id(poi) -> 8;
debug_subscription_type_id(redstone_wire_orientation) -> 9;
debug_subscription_type_id(village_section) -> 10;
debug_subscription_type_id(raid) -> 11;
debug_subscription_type_id(structure) -> 12;
debug_subscription_type_id(game_event_listener) -> 13;
debug_subscription_type_id(neighbor_update) -> 14;
debug_subscription_type_id(game_event) -> 15;
debug_subscription_type_id(#{type := Type}) -> debug_subscription_type_id(Type).






