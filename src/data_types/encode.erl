-module(encode).
-include("src/data_types/records.hrl").

-export([
	 encode_type/2,
	 encode_node/1,
	 encode_boss_bar/1,
	 encode_seen_advancements/1,
	 encode_delete_chat/1,
	 encode_chat_type/1,
	 encode_player_info_update/1,
	 encode_set_equipment/1,
	 encode_set_objective/1,
	 encode_set_player_team/1,
	 encode_waypoint_data/1,
	 encode_stop_sound/1,
	 encode_set_score/1,
	 encode_update_advancements/1
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
            encode_nbt(Data);
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
        lp_vec3 ->
            encode_lp_vec3(Data);
        node ->
            encode_node(Data);
        boss_bar ->
            encode_boss_bar(Data);
        seen_advancements ->
            encode_seen_advancements(Data);
        delete_chat ->
            encode_delete_chat(Data);
        chat_type ->
            encode_chat_type(Data);
        player_info_update ->
            encode_player_info_update(Data);
        set_equipment ->
            encode_set_equipment(Data);
        set_objective ->
            encode_set_objective(Data);
        set_player_team ->
            encode_set_player_team(Data);
        waypoint_data ->
            encode_waypoint_data(Data);
        stop_sound ->
            encode_stop_sound(Data);
        set_score ->
            encode_set_score(Data);
        update_advancements ->
            encode_update_advancements(Data);
        advancement ->
            encode_type_advancement(Data);
        advancement_progress ->
            encode_type_advancement_progress(Data);
        Types when is_list(Types) ->
            encode_tuple_elements(Data, Types);

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
encode_prefixed_array(#array{array = List}, ElemType) ->
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
encode_slot_add_components([{TypeId, ComponentData} | Rest], Acc) ->
    TypeIdBin = encode_varint(TypeId),
    CompBin   = case is_binary(ComponentData) of
                    true -> ComponentData;
                    false -> component_encode:encode_component(TypeId, ComponentData)
                end,
    encode_slot_add_components(Rest, <<Acc/binary, TypeIdBin/binary, CompBin/binary>>).


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

encode_text_component(#text_component{component_map = Map}) ->
    encode_text_component(Map);
encode_text_component(Data) when is_binary(Data) ->
    encode_string(Data);
encode_text_component(Data) when is_list(Data) ->
    case io_lib:printable_unicode_list(Data) of
        true ->
            encode_string(Data);
        false ->
            SNBTBin = text_component:to_snbt(Data),
            encode_string(SNBTBin)
    end;
encode_text_component(Data) ->
    SNBTBin = text_component:to_snbt(Data),
    encode_string(SNBTBin).

encode_json_text_component(#json_text_component{json_component_map = Map}) ->
    encode_json_text_component(Map);
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

encode_nbt(#nbt{nbt = NbtVal}) ->
    nbt:encode(NbtVal);
encode_nbt(Data) ->
    nbt:encode(Data).

extract_val(#varint{varint = V}) -> extract_val(V);
extract_val(#varlong{varlong = V}) -> extract_val(V);
extract_val(#enum{enum = V}) -> extract_val(V);
extract_val(#optional{optional = V}) -> extract_val(V);
extract_val(#byte{byte = V}) -> extract_val(V);
extract_val(#ubyte{ubyte = V}) -> extract_val(V);
extract_val(#short{short = V}) -> extract_val(V);
extract_val(#ushort{ushort = V}) -> extract_val(V);
extract_val(#int{int = V}) -> extract_val(V);
extract_val(#long{long = V}) -> extract_val(V);
extract_val(#float{float = V}) -> extract_val(V);
extract_val(#double{double = V}) -> extract_val(V);
extract_val(#string{string = V}) -> extract_val(V);
extract_val(#identifier{identifier = V}) -> extract_val(V);
extract_val(V) -> V.



encode_lp_vec3(#lp_vec3{x = X, y = Y, z = Z}) ->
    encode_lp_vec3({X, Y, Z});
encode_lp_vec3([X, Y, Z]) ->
    encode_lp_vec3({X, Y, Z});
encode_lp_vec3({X, Y, Z}) ->
    AbsX = abs(X),
    AbsY = abs(Y),
    AbsZ = abs(Z),
    MaxCoordinate = max(AbsX, max(AbsY, AbsZ)),
    case MaxCoordinate /= MaxCoordinate orelse MaxCoordinate < (1.0 / 32766.0) of
        true ->
            <<0:8>>;
        false ->
            ScaleFactor = ceil(MaxCoordinate),
            NeedContinuation = (ScaleFactor band 3) =/= ScaleFactor,
            PackedScale = case NeedContinuation of
                true -> (ScaleFactor band 3) bor 4;
                false -> ScaleFactor
            end,
            ScaleFactorD = float(ScaleFactor),
            PackX = pack_lp_vec3(X / ScaleFactorD),
            PackY = pack_lp_vec3(Y / ScaleFactorD),
            PackZ = pack_lp_vec3(Z / ScaleFactorD),
            Packed = (PackZ bsl 33) bor (PackY bsl 18) bor (PackX bsl 3) bor PackedScale,
            Byte1 = Packed band 16#FF,
            Byte2 = (Packed bsr 8) band 16#FF,
            Bytes3To6 = (Packed bsr 16) band 16#FFFFFFFF,
            Header = <<Byte1:8, Byte2:8, Bytes3To6:32/unsigned-integer-big>>,
            case NeedContinuation of
                true ->
                    ContVarInt = encode_type(ScaleFactor bsr 2, varint),
                    <<Header/binary, ContVarInt/binary>>;
                false ->
                    Header
            end
    end.

pack_lp_vec3(Val) ->
    Clamped = max(-1.0, min(1.0, float(Val))),
    round((Clamped * 0.5 + 0.5) * 32766.0) band 16#7FFF.

encode_seen_advancements(#seen_advancements{action = Action, tab_id = TabId}) ->
    ActionBin = encode_type(Action, {enum, varint}),
    TabIdBin = encode_type(TabId, {optional, identifier, extract_val(Action) =:= 0}),
    <<ActionBin/binary, TabIdBin/binary>>;
encode_seen_advancements({Action, TabId}) ->
    ActionBin = encode_type(Action, {enum, varint}),
    TabIdBin = encode_type(TabId, {optional, identifier, extract_val(Action) =:= 0}),
    <<ActionBin/binary, TabIdBin/binary>>.

encode_boss_bar(#boss_bar{
    uuid = UUID,
    action = Action,
    title = Title,
    health = Health,
    color = Color,
    division = Division,
    flags = Flags
}) ->
    UUIDBin = encode_type(UUID, uuid),
    ActionVal = extract_val(Action),
    ActionBin = encode_type(ActionVal, varint),
    Payload = case ActionVal of
        0 ->
            TitleBin = encode_type(Title, text_component),
            HealthBin = encode_type(Health, float),
            ColorBin = encode_enum(Color),
            DivisionBin = encode_enum(Division),
            FlagsBin = encode_type(Flags, ubyte),
            <<TitleBin/binary, HealthBin/binary, ColorBin/binary, DivisionBin/binary, FlagsBin/binary>>;
        1 ->
            <<>>;
        2 ->
            encode_type(Health, float);
        3 ->
            encode_type(Title, text_component);
        4 ->
            ColorBin = encode_enum(Color),
            DivisionBin = encode_enum(Division),
            <<ColorBin/binary, DivisionBin/binary>>;
        5 ->
            encode_type(Flags, ubyte)
    end,
    <<UUIDBin/binary, ActionBin/binary, Payload/binary>>.

encode_node(#node{
    flags = Flags,
    children = Children,
    redirect_node = RedirectNode,
    name = Name,
    parser_id = ParserID,
    properties = Properties,
    suggestions_type = SuggestionsType
}) ->
    FlagsVal = extract_val(Flags),
    FlagsBin = encode_byte(FlagsVal),
    ChildrenBin = encode_type(Children, {prefixed_array, varint}),
    UnsignedFlags = FlagsVal band 16#FF,
    HasRedirect = (UnsignedFlags band 16#08) =/= 0,
    RedirectBin = encode_optional(RedirectNode, varint, HasRedirect),
    NodeType = UnsignedFlags band 16#03,
    HasName = (NodeType =:= 1) orelse (NodeType =:= 2),
    NameBin = encode_optional(Name, string, HasName),
    IsArgument = (NodeType =:= 2),
    ParserIDBin = encode_optional(ParserID, varint, IsArgument),
    PropertiesBin = case IsArgument of
        true ->
            PID = extract_val(ParserID),
            encode_parser_properties(Properties, PID);
        false ->
            <<>>
    end,
    HasSuggestions = (UnsignedFlags band 16#10) =/= 0,
    SuggestionsBin = encode_optional(SuggestionsType, identifier, HasSuggestions),
    <<FlagsBin/binary, ChildrenBin/binary, RedirectBin/binary, NameBin/binary, ParserIDBin/binary, PropertiesBin/binary, SuggestionsBin/binary>>;
encode_node({Flags, Children, RedirectNode, Name, ParserID, Properties, SuggestionsType}) ->
    encode_node(#node{
        flags = Flags,
        children = Children,
        redirect_node = RedirectNode,
        name = Name,
        parser_id = ParserID,
        properties = Properties,
        suggestions_type = SuggestionsType
    });
encode_node({Flags, _ChildrenCount, Children, RedirectNode, Name, ParserID, Properties, SuggestionsType}) ->
    encode_node(#node{
        flags = Flags,
        children = Children,
        redirect_node = RedirectNode,
        name = Name,
        parser_id = ParserID,
        properties = Properties,
        suggestions_type = SuggestionsType
    }).

encode_parser_properties(Properties0, 1) -> % brigadier:float
    Properties = extract_val(Properties0),
    {Flags0, Min0, Max0} = case Properties of
        {F, Mi, Ma} -> {F, Mi, Ma};
        _ -> {0, undefined, undefined}
    end,
    Flags = extract_val(Flags0),
    Min = extract_val(Min0),
    Max = extract_val(Max0),
    FlagsBin = encode_byte(Flags),
    MinBin = encode_optional(Min, float, (Flags band 16#01) =/= 0),
    MaxBin = encode_optional(Max, float, (Flags band 16#02) =/= 0),
    <<FlagsBin/binary, MinBin/binary, MaxBin/binary>>;
encode_parser_properties(Properties0, 2) -> % brigadier:double
    Properties = extract_val(Properties0),
    {Flags0, Min0, Max0} = case Properties of
        {F, Mi, Ma} -> {F, Mi, Ma};
        _ -> {0, undefined, undefined}
    end,
    Flags = extract_val(Flags0),
    Min = extract_val(Min0),
    Max = extract_val(Max0),
    FlagsBin = encode_byte(Flags),
    MinBin = encode_optional(Min, double, (Flags band 16#01) =/= 0),
    MaxBin = encode_optional(Max, double, (Flags band 16#02) =/= 0),
    <<FlagsBin/binary, MinBin/binary, MaxBin/binary>>;
encode_parser_properties(Properties0, 3) -> % brigadier:integer
    Properties = extract_val(Properties0),
    {Flags0, Min0, Max0} = case Properties of
        {F, Mi, Ma} -> {F, Mi, Ma};
        _ -> {0, undefined, undefined}
    end,
    Flags = extract_val(Flags0),
    Min = extract_val(Min0),
    Max = extract_val(Max0),
    FlagsBin = encode_byte(Flags),
    MinBin = encode_optional(Min, int, (Flags band 16#01) =/= 0),
    MaxBin = encode_optional(Max, int, (Flags band 16#02) =/= 0),
    <<FlagsBin/binary, MinBin/binary, MaxBin/binary>>;
encode_parser_properties(Properties0, 4) -> % brigadier:long
    Properties = extract_val(Properties0),
    {Flags0, Min0, Max0} = case Properties of
        {F, Mi, Ma} -> {F, Mi, Ma};
        _ -> {0, undefined, undefined}
    end,
    Flags = extract_val(Flags0),
    Min = extract_val(Min0),
    Max = extract_val(Max0),
    FlagsBin = encode_byte(Flags),
    MinBin = encode_optional(Min, long, (Flags band 16#01) =/= 0),
    MaxBin = encode_optional(Max, long, (Flags band 16#02) =/= 0),
    <<FlagsBin/binary, MinBin/binary, MaxBin/binary>>;
encode_parser_properties(Behavior, 5) -> % brigadier:string
    encode_type(extract_val(Behavior), varint);
encode_parser_properties(Flags, 6) -> % minecraft:entity
    encode_byte(extract_val(Flags));
encode_parser_properties(Min, 23) -> % minecraft:time
    encode_int(extract_val(Min));
encode_parser_properties(Registry, PID) when PID >= 24, PID =< 27 -> % resource/tag
    encode_identifier(extract_val(Registry));
encode_parser_properties(Flags, 34) -> % minecraft:score_holder
    encode_byte(extract_val(Flags));
encode_parser_properties(_Properties, _PID) ->
    <<>>.


encode_delete_chat(#delete_chat{
    message_id = MsgId,
    signature = Signature
}) ->
    MsgIdVal = extract_val(MsgId),
    MsgIdBin = encode_varint(MsgIdVal),
    IsTrue = (MsgIdVal =:= 0),
    SignatureBin = encode_optional(Signature, {byte_array, 256}, IsTrue),
    <<MsgIdBin/binary, SignatureBin/binary>>.

encode_chat_type(#chat_type{
    translation_key = TranslationKey,
    parameters = Parameters,
    style = Style
}) ->
    TransKeyBin = encode_type(TranslationKey, string),
    ParamsBin = encode_type(Parameters, {prefixed_array, {enum, [sender, target, content]}}),
    StyleBin = encode_type(Style, nbt),
    <<TransKeyBin/binary, ParamsBin/binary, StyleBin/binary>>;
encode_chat_type({TranslationKey, Parameters, Style}) ->
    TransKeyBin = encode_type(TranslationKey, string),
    ParamsBin = encode_type(Parameters, {prefixed_array, {enum, [sender, target, content]}}),
    StyleBin = encode_type(Style, nbt),
    <<TransKeyBin/binary, ParamsBin/binary, StyleBin/binary>>.

encode_player_info_update(#player_info_update{actions = Actions, players = Players}) ->
    encode_player_info_update(Actions, Players);
encode_player_info_update({Actions, Players}) ->
    encode_player_info_update(Actions, Players).

encode_player_info_update(Actions, Players) ->
    ActionsVal = extract_val(Actions),
    ActionsBin = encode_type(ActionsVal, ubyte),
    CountBin = encode_type(length(Players), varint),
    PlayersBin = iolist_to_binary([encode_player_info_entry(Entry, ActionsVal) || Entry <- Players]),
    <<ActionsBin/binary, CountBin/binary, PlayersBin/binary>>.

encode_player_info_entry(#player_info_entry{uuid = UUID, actions = Actions}, ActionsMask) ->
    UUIDBin = encode_type(UUID, uuid),
    ActionsBin = encode_player_actions(Actions, ActionsMask),
    <<UUIDBin/binary, ActionsBin/binary>>;
encode_player_info_entry({UUID, Actions}, ActionsMask) ->
    UUIDBin = encode_type(UUID, uuid),
    ActionsBin = encode_player_actions(Actions, ActionsMask),
    <<UUIDBin/binary, ActionsBin/binary>>.

encode_player_actions(ActionsList, ActionsMask) ->
    ActionBits = [
        {16#01, add_player, fun encode_action_add_player/1},
        {16#02, initialize_chat, fun encode_action_initialize_chat/1},
        {16#04, update_game_mode, fun encode_action_update_game_mode/1},
        {16#08, update_listed, fun encode_action_update_listed/1},
        {16#10, update_latency, fun encode_action_update_latency/1},
        {16#20, update_display_name, fun encode_action_update_display_name/1},
        {16#40, update_list_order, fun encode_action_update_list_order/1}
    ],
    encode_player_actions_loop(ActionsList, ActionsMask, ActionBits, []).

encode_player_actions_loop(_ActionsList, _ActionsMask, [], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
encode_player_actions_loop(ActionsList, ActionsMask, [{Bit, Tag, EncodeFun} | Rest], Acc) ->
    case (ActionsMask band Bit) =/= 0 of
        true ->
            {ActionVal, RemainingList} = get_action_val(Tag, ActionsList),
            Bin = EncodeFun(ActionVal),
            encode_player_actions_loop(RemainingList, ActionsMask, Rest, [Bin | Acc]);
        false ->
            encode_player_actions_loop(ActionsList, ActionsMask, Rest, Acc)
    end.

get_action_val(Tag, ActionsList) ->
    case lists:keyfind(Tag, 1, ActionsList) of
        {Tag, Arg1, Arg2} ->
            {{Tag, Arg1, Arg2}, lists:keydelete(Tag, 1, ActionsList)};
        {Tag, Arg1} ->
            {Arg1, lists:keydelete(Tag, 1, ActionsList)};
        false ->
            case ActionsList of
                [Head | Tail] -> {Head, Tail};
                [] -> {undefined, []}
            end
    end.

encode_action_add_player({add_player, Name, Properties}) ->
    encode_action_add_player({Name, Properties});
encode_action_add_player({Name, Properties}) ->
    NameBin = encode_type(Name, string),
    CountBin = encode_type(length(Properties), varint),
    PropsBin = encode_properties(Properties),
    <<NameBin/binary, CountBin/binary, PropsBin/binary>>.

encode_action_initialize_chat({initialize_chat, ChatSession}) ->
    encode_action_initialize_chat(ChatSession);
encode_action_initialize_chat(ChatSession) ->
    encode_prefixed_optional(ChatSession, [uuid, long, byte_array, byte_array]).

encode_action_update_game_mode({update_game_mode, GameMode}) ->
    encode_action_update_game_mode(GameMode);
encode_action_update_game_mode(GameMode) ->
    encode_type(GameMode, varint).

encode_action_update_listed({update_listed, Listed}) ->
    encode_action_update_listed(Listed);
encode_action_update_listed(Listed) ->
    encode_type(Listed, bool).

encode_action_update_latency({update_latency, Latency}) ->
    encode_action_update_latency(Latency);
encode_action_update_latency(Latency) ->
    encode_type(Latency, varint).

encode_action_update_display_name({update_display_name, DisplayName}) ->
    encode_action_update_display_name(DisplayName);
encode_action_update_display_name(DisplayName) ->
    encode_prefixed_optional(DisplayName, json_text_component).

encode_action_update_list_order({update_list_order, ListOrder}) ->
    encode_action_update_list_order(ListOrder);
encode_action_update_list_order(ListOrder) ->
    encode_type(ListOrder, varint).

encode_set_equipment(#set_equipment{entity_id = EntityID, equipment = Equipment}) ->
    encode_set_equipment(EntityID, Equipment);
encode_set_equipment({EntityID, Equipment}) ->
    encode_set_equipment(EntityID, Equipment).

encode_set_equipment(EntityID, Equipment) ->
    EntityIDBin = encode_type(EntityID, varint),
    EquipmentBin = encode_equipment_list(Equipment),
    <<EntityIDBin/binary, EquipmentBin/binary>>.

encode_equipment_list([]) ->
    <<>>;
encode_equipment_list([Entry | Rest]) ->
    HasNext = Rest =/= [],
    {Slot, Item} = case Entry of
        {S, I} -> {S, I};
        _ -> error({invalid_equipment_entry, Entry})
    end,
    SlotVal = extract_val(Slot),
    RawByte = case HasNext of
        true -> (SlotVal band 16#7F) bor 16#80;
        false -> SlotVal band 16#7F
    end,
    SlotBin = encode_byte(RawByte),
    ItemBin = encode_type(Item, slot),
    RestBin = encode_equipment_list(Rest),
    <<SlotBin/binary, ItemBin/binary, RestBin/binary>>.

encode_set_objective(#set_objective{
    objective_name = ObjectiveName,
    mode = Mode,
    objective_value = ObjectiveValue,
    type = Type,
    number_format = NumberFormat
}) ->
    encode_set_objective(ObjectiveName, Mode, ObjectiveValue, Type, NumberFormat);
encode_set_objective({ObjectiveName, Mode, ObjectiveValue, Type, NumberFormat}) ->
    encode_set_objective(ObjectiveName, Mode, ObjectiveValue, Type, NumberFormat);
encode_set_objective({ObjectiveName, Mode}) ->
    encode_set_objective(ObjectiveName, Mode, undefined, undefined, undefined).

encode_set_objective(ObjectiveName, Mode0, ObjectiveValue, Type0, NumberFormat) ->
    NameBin = encode_type(ObjectiveName, string),
    Mode = extract_val(Mode0),
    ModeBin = encode_type(Mode, byte),
    Payload = case Mode of
        1 ->
            <<>>;
        _ when Mode =:= 0; Mode =:= 2 ->
            ValueBin = encode_type(ObjectiveValue, text_component),
            TypeVal = case extract_val(Type0) of
                integer -> 0;
                hearts -> 1;
                OtherType -> extract_val(OtherType)
            end,
            TypeBin = encode_enum(TypeVal, varint),
            case NumberFormat of
                undefined ->
                    HasNFBin = encode_type(false, bool),
                    <<ValueBin/binary, TypeBin/binary, HasNFBin/binary>>;
                false ->
                    HasNFBin = encode_type(false, bool),
                    <<ValueBin/binary, TypeBin/binary, HasNFBin/binary>>;
                none ->
                    HasNFBin = encode_type(false, bool),
                    <<ValueBin/binary, TypeBin/binary, HasNFBin/binary>>;
                _ ->
                    HasNFBin = encode_type(true, bool),
                    NFBin = encode_number_format(NumberFormat),
                    <<ValueBin/binary, TypeBin/binary, HasNFBin/binary, NFBin/binary>>
            end
    end,
    <<NameBin/binary, ModeBin/binary, Payload/binary>>.

encode_number_format(blank) ->
    encode_enum(0, varint);
encode_number_format(0) ->
    encode_enum(0, varint);
encode_number_format({blank}) ->
    encode_enum(0, varint);
encode_number_format({0}) ->
    encode_enum(0, varint);
encode_number_format({blank, _}) ->
    encode_enum(0, varint);
encode_number_format({0, _}) ->
    encode_enum(0, varint);
encode_number_format({styled, Styling}) ->
    FormatIDBin = encode_enum(1, varint),
    StylingBin = encode_type(Styling, nbt),
    <<FormatIDBin/binary, StylingBin/binary>>;
encode_number_format({1, Styling}) ->
    FormatIDBin = encode_enum(1, varint),
    StylingBin = encode_type(Styling, nbt),
    <<FormatIDBin/binary, StylingBin/binary>>;
encode_number_format({fixed, Content}) ->
    FormatIDBin = encode_enum(2, varint),
    ContentBin = encode_type(Content, text_component),
    <<FormatIDBin/binary, ContentBin/binary>>;
encode_number_format({2, Content}) ->
    FormatIDBin = encode_enum(2, varint),
    ContentBin = encode_type(Content, text_component),
    <<FormatIDBin/binary, ContentBin/binary>>.

encode_set_player_team(#set_player_team{
    team_name = TeamName,
    method = Method,
    team_display_name = TeamDisplayName,
    team_prefix = TeamPrefix,
    team_suffix = TeamSuffix,
    name_tag_visibility = NameTagVis,
    collision_rule = CollisionRule,
    team_color = TeamColor,
    friendly_flags = FriendlyFlags,
    entities = Entities
}) ->
    encode_set_player_team(TeamName, Method, TeamDisplayName, TeamPrefix, TeamSuffix, NameTagVis, CollisionRule, TeamColor, FriendlyFlags, Entities);
encode_set_player_team({TeamName, Method, TeamDisplayName, TeamPrefix, TeamSuffix, NameTagVis, CollisionRule, TeamColor, FriendlyFlags, Entities}) ->
    encode_set_player_team(TeamName, Method, TeamDisplayName, TeamPrefix, TeamSuffix, NameTagVis, CollisionRule, TeamColor, FriendlyFlags, Entities);
encode_set_player_team({TeamName, Method}) ->
    encode_set_player_team(TeamName, Method, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined).

encode_set_player_team(TeamName, Method0, TeamDisplayName, TeamPrefix, TeamSuffix, NameTagVis, CollisionRule, TeamColor, FriendlyFlags, Entities) ->
    NameBin = encode_type(TeamName, string),
    Method = extract_val(Method0),
    MethodBin = encode_type(Method, byte),
    Payload = case Method of
        0 ->
            DispBin = encode_type(TeamDisplayName, text_component),
            PrefBin = encode_type(TeamPrefix, text_component),
            SuffBin = encode_type(TeamSuffix, text_component),
            VisVal = encode_team_name_tag_visibility(NameTagVis),
            VisBin = encode_enum(VisVal, varint),
            CollVal = encode_team_collision_rule(CollisionRule),
            CollBin = encode_enum(CollVal, varint),
            ColVal = encode_team_color(TeamColor),
            ColBin = encode_enum(ColVal, varint),
            FlagsVal = extract_val(FriendlyFlags),
            FlagsBin = encode_type(FlagsVal, byte),
            EntList = extract_entities_list(Entities),
            EntBin = encode_type(EntList, {prefixed_array, string}),
            <<DispBin/binary, PrefBin/binary, SuffBin/binary, VisBin/binary, CollBin/binary, ColBin/binary, FlagsBin/binary, EntBin/binary>>;
        1 ->
            <<>>;
        2 ->
            DispBin = encode_type(TeamDisplayName, text_component),
            PrefBin = encode_type(TeamPrefix, text_component),
            SuffBin = encode_type(TeamSuffix, text_component),
            VisVal = encode_team_name_tag_visibility(NameTagVis),
            VisBin = encode_enum(VisVal, varint),
            CollVal = encode_team_collision_rule(CollisionRule),
            CollBin = encode_enum(CollVal, varint),
            ColVal = encode_team_color(TeamColor),
            ColBin = encode_enum(ColVal, varint),
            FlagsVal = extract_val(FriendlyFlags),
            FlagsBin = encode_type(FlagsVal, byte),
            <<DispBin/binary, PrefBin/binary, SuffBin/binary, VisBin/binary, CollBin/binary, ColBin/binary, FlagsBin/binary>>;
        3 ->
            EntList = extract_entities_list(Entities),
            encode_type(EntList, {prefixed_array, string});
        4 ->
            EntList = extract_entities_list(Entities),
            encode_type(EntList, {prefixed_array, string})
    end,
    <<NameBin/binary, MethodBin/binary, Payload/binary>>.

extract_entities_list(#prefixed_array{prefixed_array = List}) -> List;
extract_entities_list(List) when is_list(List) -> List;
extract_entities_list(undefined) -> [].

encode_team_name_tag_visibility(always) -> 0;
encode_team_name_tag_visibility("always") -> 0;
encode_team_name_tag_visibility(never) -> 1;
encode_team_name_tag_visibility("never") -> 1;
encode_team_name_tag_visibility(hide_for_other_teams) -> 2;
encode_team_name_tag_visibility("hide_for_other_teams") -> 2;
encode_team_name_tag_visibility(hide_other_teams) -> 2;
encode_team_name_tag_visibility("hide_other_teams") -> 2;
encode_team_name_tag_visibility(hide_for_own_teams) -> 3;
encode_team_name_tag_visibility("hide_for_own_teams") -> 3;
encode_team_name_tag_visibility(hide_own_team) -> 3;
encode_team_name_tag_visibility("hide_own_team") -> 3;
encode_team_name_tag_visibility(V) -> extract_val(V).

encode_team_collision_rule(always) -> 0;
encode_team_collision_rule("always") -> 0;
encode_team_collision_rule(never) -> 1;
encode_team_collision_rule("never") -> 1;
encode_team_collision_rule(push_other_teams) -> 2;
encode_team_collision_rule("push_other_teams") -> 2;
encode_team_collision_rule(push_other) -> 2;
encode_team_collision_rule("push_other") -> 2;
encode_team_collision_rule(push_own_team) -> 3;
encode_team_collision_rule("push_own_team") -> 3;
encode_team_collision_rule(push_own) -> 3;
encode_team_collision_rule("push_own") -> 3;
encode_team_collision_rule(V) -> extract_val(V).

encode_team_color(black) -> 0;
encode_team_color(dark_blue) -> 1;
encode_team_color(dark_green) -> 2;
encode_team_color(dark_aqua) -> 3;
encode_team_color(dark_red) -> 4;
encode_team_color(dark_purple) -> 5;
encode_team_color(gold) -> 6;
encode_team_color(gray) -> 7;
encode_team_color(dark_gray) -> 8;
encode_team_color(blue) -> 9;
encode_team_color(green) -> 10;
encode_team_color(aqua) -> 11;
encode_team_color(red) -> 12;
encode_team_color(light_purple) -> 13;
encode_team_color(yellow) -> 14;
encode_team_color(white) -> 15;
encode_team_color(obfuscated) -> 16;
encode_team_color(bold) -> 17;
encode_team_color(strikethrough) -> 18;
encode_team_color(underlined) -> 19;
encode_team_color(italic) -> 20;
encode_team_color(reset) -> 21;
encode_team_color(V) -> extract_val(V).

encode_waypoint_data(#waypoint_data{
    waypoint_type = Type,
    x = X,
    y = Y,
    z = Z,
    angle = Angle
}) ->
    encode_waypoint_data(Type, X, Y, Z, Angle);
encode_waypoint_data({waypoint_data, Type, X, Z}) ->
    encode_waypoint_data(Type, X, undefined, Z, undefined);
encode_waypoint_data({waypoint_data, Type, Angle}) ->
    encode_waypoint_data(Type, undefined, undefined, undefined, Angle);
encode_waypoint_data({waypoint_data, Type}) ->
    encode_waypoint_data(Type, undefined, undefined, undefined, undefined);
encode_waypoint_data({Type, X, Y, Z}) ->
    encode_waypoint_data(Type, X, Y, Z, undefined);
encode_waypoint_data({Type, X, Z}) ->
    encode_waypoint_data(Type, X, undefined, Z, undefined);
encode_waypoint_data({Type, Angle}) ->
    encode_waypoint_data(Type, undefined, undefined, undefined, Angle);
encode_waypoint_data(Type) when is_integer(Type); is_atom(Type) ->
    encode_waypoint_data(Type, undefined, undefined, undefined, undefined).

encode_waypoint_data(Type0, X0, Y0, Z0, Angle0) ->
    TypeVal = case extract_val(Type0) of
        0 -> 0;
        empty -> 0;
        1 -> 1;
        vec3i -> 1;
        2 -> 2;
        chunk -> 2;
        3 -> 3;
        azimuth -> 3;
        Other -> extract_val(Other)
    end,
    TypeBin = encode_enum(TypeVal, varint),
    PayloadBin = case TypeVal of
        0 ->
            <<>>;
        1 ->
            XBin = encode_type(extract_val(X0), varint),
            YBin = encode_type(extract_val(Y0), varint),
            ZBin = encode_type(extract_val(Z0), varint),
            <<XBin/binary, YBin/binary, ZBin/binary>>;
        2 ->
            XBin = encode_type(extract_val(X0), varint),
            ZBin = encode_type(extract_val(Z0), varint),
            <<XBin/binary, ZBin/binary>>;
        3 ->
            AngleBin = encode_type(extract_val(Angle0), float),
            <<AngleBin/binary>>;
        _ ->
            error({unknown_waypoint_type, TypeVal})
    end,
    <<TypeBin/binary, PayloadBin/binary>>.

encode_stop_sound(#stop_sound{
    flags = Flags,
    source = Source,
    sound = Sound
}) ->
    encode_stop_sound(Flags, Source, Sound);
encode_stop_sound({stop_sound, Source, Sound}) ->
    encode_stop_sound(undefined, Source, Sound);
encode_stop_sound({stop_sound, Source}) ->
    encode_stop_sound(undefined, Source, undefined);
encode_stop_sound({Source, Sound}) ->
    encode_stop_sound(undefined, Source, Sound);
encode_stop_sound(Map) when is_map(Map) ->
    Flags = maps:get(flags, Map, undefined),
    Source = maps:get(source, Map, undefined),
    Sound = maps:get(sound, Map, undefined),
    encode_stop_sound(Flags, Source, Sound);
encode_stop_sound(Source) when is_atom(Source); is_integer(Source); is_binary(Source); is_tuple(Source) ->
    encode_stop_sound(undefined, Source, undefined).

encode_stop_sound(Flags0, Source0, Sound0) ->
    HasSource = (Source0 =/= undefined andalso Source0 =/= none),
    HasSound = (Sound0 =/= undefined andalso Sound0 =/= none),
    FlagsVal = case Flags0 of
        undefined ->
            (if HasSource -> 1; true -> 0 end) bor (if HasSound -> 2; true -> 0 end);
        _ ->
            extract_val(Flags0)
    end,
    FlagsBin = encode_type(FlagsVal, byte),
    SourceBin = case (FlagsVal band 1) =/= 0 of
        true ->
            SourceVal = encode_stop_sound_source(extract_val(Source0)),
            encode_type(SourceVal, {enum, varint});
        false ->
            <<>>
    end,
    SoundBin = case (FlagsVal band 2) =/= 0 of
        true ->
            encode_type(Sound0, identifier);
        false ->
            <<>>
    end,
    <<FlagsBin/binary, SourceBin/binary, SoundBin/binary>>.

encode_stop_sound_source(master) -> 0;
encode_stop_sound_source(music) -> 1;
encode_stop_sound_source(record) -> 2;
encode_stop_sound_source(weather) -> 3;
encode_stop_sound_source(block) -> 4;
encode_stop_sound_source(hostile) -> 5;
encode_stop_sound_source(neutral) -> 6;
encode_stop_sound_source(player) -> 7;
encode_stop_sound_source(ambient) -> 8;
encode_stop_sound_source(voice) -> 9;
encode_stop_sound_source(V) -> extract_val(V).

encode_set_score(#set_score{
    entity_name = EntityName,
    objective_name = ObjectiveName,
    value = Value,
    display_name = DisplayName,
    number_format = NumberFormat
}) ->
    encode_set_score(EntityName, ObjectiveName, Value, DisplayName, NumberFormat);
encode_set_score({EntityName, ObjectiveName, Value, DisplayName, NumberFormat}) ->
    encode_set_score(EntityName, ObjectiveName, Value, DisplayName, NumberFormat);
encode_set_score({EntityName, ObjectiveName, Value}) ->
    encode_set_score(EntityName, ObjectiveName, Value, undefined, undefined).

encode_set_score(EntityName, ObjectiveName, Value, DisplayName, NumberFormat) ->
    EntityBin = encode_type(EntityName, string),
    ObjectiveBin = encode_type(ObjectiveName, string),
    ValueBin = encode_type(Value, varint),
    DisplayBin = case DisplayName of
        undefined -> encode_type(false, bool);
        false -> encode_type(false, bool);
        none -> encode_type(false, bool);
        _ ->
            HasDNBin = encode_type(true, bool),
            DNBin = encode_type(DisplayName, text_component),
            <<HasDNBin/binary, DNBin/binary>>
    end,
    FormatBin = case NumberFormat of
        undefined -> encode_type(false, bool);
        false -> encode_type(false, bool);
        none -> encode_type(false, bool);
        _ ->
            HasNFBin = encode_type(true, bool),
            NFBin = encode_number_format(NumberFormat),
            <<HasNFBin/binary, NFBin/binary>>
    end,
    <<EntityBin/binary, ObjectiveBin/binary, ValueBin/binary, DisplayBin/binary, FormatBin/binary>>.

encode_update_advancements(#update_advancements{
    reset = Reset,
    advancement_mapping = AdvancementMapping,
    identifiers = Identifiers,
    progress_mapping = ProgressMapping
}) ->
    encode_update_advancements(Reset, AdvancementMapping, Identifiers, ProgressMapping);
encode_update_advancements({Reset, AdvancementMapping, Identifiers, ProgressMapping}) ->
    encode_update_advancements(Reset, AdvancementMapping, Identifiers, ProgressMapping).

encode_update_advancements(Reset, AdvancementMapping, Identifiers, ProgressMapping) ->
    ResetBin = encode_bool(Reset),
    MappingBin = encode_prefixed_array(AdvancementMapping, [identifier, advancement]),
    IdentBin  = encode_prefixed_array(Identifiers, identifier),
    ProgBin   = encode_prefixed_array(ProgressMapping, [identifier, advancement_progress]),
    <<ResetBin/binary, MappingBin/binary, IdentBin/binary, ProgBin/binary>>.

encode_type_advancement(#advancement{
    parent_id      = ParentId,
    display_data   = DisplayData,
    requirements   = Requirements,
    sends_telemetry = SendsTelemetry
}) ->
    encode_type_advancement(ParentId, DisplayData, Requirements, SendsTelemetry);
encode_type_advancement({ParentId, DisplayData, Requirements, SendsTelemetry}) ->
    encode_type_advancement(ParentId, DisplayData, Requirements, SendsTelemetry).

encode_type_advancement(ParentId, DisplayData, Requirements, SendsTelemetry) ->
    ParentBin   = encode_prefixed_optional(ParentId, identifier),
    DisplayBin  = encode_prefixed_optional_advancement_display(DisplayData),
    ReqBin      = encode_prefixed_array(Requirements, {prefixed_array, string}),
    TelBin      = encode_bool(SendsTelemetry),
    <<ParentBin/binary, DisplayBin/binary, ReqBin/binary, TelBin/binary>>.

encode_prefixed_optional_advancement_display(none) ->
    encode_bool(false);
encode_prefixed_optional_advancement_display(undefined) ->
    encode_bool(false);
encode_prefixed_optional_advancement_display({some, Display}) ->
    <<(encode_bool(true))/binary, (encode_type_advancement_display(Display))/binary>>;
encode_prefixed_optional_advancement_display(#advancement_display{} = Display) ->
    <<(encode_bool(true))/binary, (encode_type_advancement_display(Display))/binary>>;
encode_prefixed_optional_advancement_display(Display) when is_tuple(Display); is_map(Display) ->
    <<(encode_bool(true))/binary, (encode_type_advancement_display(Display))/binary>>.

encode_type_advancement_display(#advancement_display{
    title              = Title,
    description        = Description,
    icon               = Icon,
    frame_type         = FrameType,
    flags              = Flags,
    background_texture = BgTexture,
    x                  = X,
    y                  = Y
}) ->
    encode_type_advancement_display(Title, Description, Icon, FrameType, Flags, BgTexture, X, Y);
encode_type_advancement_display({Title, Description, Icon, FrameType, Flags, BgTexture, X, Y}) ->
    encode_type_advancement_display(Title, Description, Icon, FrameType, Flags, BgTexture, X, Y).

encode_type_advancement_display(Title, Description, Icon, FrameType, Flags0, BgTexture, X, Y) ->
    Flags = extract_val(Flags0),
    TitleBin  = encode_text_component(Title),
    DescBin   = encode_text_component(Description),
    IconBin   = encode_slot(Icon),
    FrameBin  = encode_varint(FrameType),
    FlagsBin  = encode_int(Flags),
    BgBin     = case (Flags band 16#01) =/= 0 of
                    true  -> encode_identifier(BgTexture);
                    false -> <<>>
                end,
    XBin = encode_float(X),
    YBin = encode_float(Y),
    <<TitleBin/binary, DescBin/binary, IconBin/binary, FrameBin/binary,
      FlagsBin/binary, BgBin/binary, XBin/binary, YBin/binary>>.

encode_type_advancement_progress(#advancement_progress{criteria = Criteria}) ->
    encode_type_advancement_progress(Criteria);
encode_type_advancement_progress(Criteria) when is_list(Criteria) ->
    encode_prefixed_array(Criteria, [identifier, {prefixed_optional, long}]).

%% Hook advancement and advancement_progress into encode_type/2
%% (these are only used via encode_prefixed_array_list, so we add them here for completeness)
encode_type_advancement_hook(Data, advancement) ->
    encode_type_advancement(Data);
encode_type_advancement_hook(Data, advancement_progress) ->
    encode_type_advancement_progress(Data).


