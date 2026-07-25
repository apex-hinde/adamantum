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
        varlong ->
            decode_varlong(Data);
%%        entity_metadata ->
%%            decode_entity_metadata(Data);
        slot ->
            decode_slot(Data);
        hashed_slot ->
            decode_hashed_slot(Data);
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
        {optional, Inner_Type, Bool} ->
            decode_optional(Data, Inner_Type, Bool);
        {optional, Inner_Type} ->
            decode_prefixed_optional(Data, Inner_Type);
        {array, Arg1, Arg2} ->
            decode_array(Data, Arg1, Arg2);
        {prefixed_array, ElemType} ->
            decode_prefixed_array(Data, ElemType);
        {prefixed_array, PrefixType, ElemType} ->
            decode_prefixed_array(Data, PrefixType, ElemType);
        enum ->
            decode_enum(Data);
        {enum, Arg1} ->
            decode_enum(Data, Arg1);
        {enum, Arg1, Arg2} ->
            decode_enum(Data, Arg1, Arg2);
        byte_array ->
            decode_byte_array(Data);
        {byte_array, Arg1} ->
            decode_byte_array(Data, Arg1)
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

decode_varint(_Data, Position, _Acc) when Position >= 35 ->
    {error, "varint too big"};
decode_varint(<<1:1, Number:7, Rest/binary>>, Position, Acc) ->
    decode_varint(Rest, Position + 7, (Number bsl Position) + Acc);
decode_varint(<<0:1, Number:7, Rest/binary>>, Position, Acc) ->
    RawVal = (Number bsl Position) + Acc,
    Val = case RawVal band 16#FFFFFFFF of
        V when V >= 16#80000000 -> V - 16#100000000;
        V -> V
    end,
    {Val, Rest};
decode_varint(<<>>, _, _) ->
    {error, "insufficient data"}.

decode_varlong(Data) when is_binary(Data) ->
    decode_varlong(Data, 0, 0).

decode_varlong(_Data, Position, _Acc) when Position >= 70 ->
    {error, "varlong too big"};
decode_varlong(<<1:1, Number:7, Rest/binary>>, Position, Acc) ->
    decode_varlong(Rest, Position + 7, (Number bsl Position) + Acc);
decode_varlong(<<0:1, Number:7, Rest/binary>>, Position, Acc) ->
    RawVal = (Number bsl Position) + Acc,
    Val = case RawVal band 16#FFFFFFFFFFFFFFFF of
        V when V >= 16#8000000000000000 -> V - 16#10000000000000000;
        V -> V
    end,
    {Val, Rest};
decode_varlong(<<>>, _, _) ->
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

decode_byte_array(Data) ->
    {<<>>, Data}.

decode_byte_array(Data, Length) when is_integer(Length) ->
    <<ByteArray:Length/binary, Data2/binary>> = Data,
    {Data2, ByteArray};
decode_byte_array(Data, PrefixType) when is_atom(PrefixType) ->
    {RestData, Length} = decode_type_internal(Data, PrefixType),
    <<ByteArray:Length/binary, Data2/binary>> = RestData,
    {Data2, ByteArray}.



decode_optional(Data, Type, true) ->
    {RestData, Value} = decode_type(Data, Type),
    {RestData, {some, Value}};

decode_optional(Data, _Type, false) ->
    {Data, none}.

decode_prefixed_optional(Data, InnerType) ->
    {RestData, IsPresent} = decode_bool(Data),
    case IsPresent of
        true ->
            {RestData2, Value} = decode_type(RestData, InnerType),
            {RestData2, {some, Value}};
        false ->
            {RestData, none}
    end.

decode_array(Data, Count, ElemType) when is_integer(Count), Count >= 0 ->
    decode_array_loop(Data, Count, ElemType, []);
decode_array(Data, ElemType, Count) when is_integer(Count), Count >= 0 ->
    decode_array_loop(Data, Count, ElemType, []).

decode_array_loop(Data, 0, _ElemType, Acc) ->
    {Data, lists:reverse(Acc)};
decode_array_loop(Data, Count, ElemType, Acc) ->
    {RestData, Elem} = decode_type(Data, ElemType),
    decode_array_loop(RestData, Count - 1, ElemType, [Elem | Acc]).

decode_prefixed_array(Data, ElemType) ->
    {Count, RestData} = decode_varint(Data),
    decode_array(RestData, Count, ElemType).

decode_prefixed_array(Data, varint, ElemType) ->
    {Count, RestData} = decode_varint(Data),
    decode_array(RestData, Count, ElemType);
decode_prefixed_array(Data, PrefixType, ElemType) ->
    {RestData, Count} = decode_type(Data, PrefixType),
    decode_array(RestData, Count, ElemType).

decode_enum(Data) ->
    decode_enum(Data, varint).

decode_enum(Data, InnerType) when is_atom(InnerType) ->
    decode_type_internal(Data, InnerType);
decode_enum(Data, EnumList) when is_list(EnumList); is_map(EnumList) ->
    decode_enum(Data, varint, EnumList).

decode_enum(Data, InnerType, EnumList) ->
    case decode_type_internal(Data, InnerType) of
        {error, Reason} ->
            {error, Reason};
        {Rest, Val} when is_list(EnumList) ->
            if is_integer(Val) andalso Val >= 0 andalso Val < length(EnumList) ->
                    {Rest, lists:nth(Val + 1, EnumList)};
               true ->
                    case lists:member(Val, EnumList) of
                        true -> {Rest, Val};
                        false -> {error, "invalid enum value"}
                    end
            end;
        {Rest, Val} when is_map(EnumList) ->
            case EnumList of
       #{Val := MappedVal} ->
           {Rest, MappedVal};
       #{} ->
           case lists:member(Val, maps:values(EnumList)) of
                        true -> {Rest, Val};
                        false -> {error, "invalid enum value"}
                    end
   end
    end.

decode_type_internal(Data, varint) ->
    case decode_varint(Data) of
        {error, Err} -> {error, Err};
        {Val, Rest} -> {Rest, Val}
    end;
decode_type_internal(Data, varlong) ->
    case decode_varlong(Data) of
        {error, Err} -> {error, Err};
        {Val, Rest} -> {Rest, Val}
    end;
decode_type_internal(Data, InnerType) ->
    decode_type(Data, InnerType).

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
decode_slot(Data) ->
    {ItemCount, Rest1} = decode_varint(Data),
    case ItemCount of
        0 ->
            {Rest1, empty};
        _ ->
            {ItemID,  Rest2} = decode_varint(Rest1),
            {NAdd,    Rest3} = decode_varint(Rest2),
            {NRemove, Rest4} = decode_varint(Rest3),
            {Rest5, ComponentsToAdd}    = decode_slot_add_components(Rest4, NAdd, []),
            {Rest6, ComponentsToRemove} = decode_slot_remove_components(Rest5, NRemove, []),
            {Rest6, {ItemCount, ItemID, ComponentsToAdd, ComponentsToRemove}}
    end.

decode_slot_add_components(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_slot_add_components(Data, N, Acc) ->
    {TypeId,  Rest1} = decode_varint(Data),
    {DataLen, Rest2} = decode_varint(Rest1),
    <<DataBin:DataLen/binary, Rest3/binary>> = Rest2,
    decode_slot_add_components(Rest3, N - 1, [{TypeId, DataBin} | Acc]).

decode_slot_remove_components(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_slot_remove_components(Data, N, Acc) ->
    {TypeId, Rest1} = decode_varint(Data),
    decode_slot_remove_components(Rest1, N - 1, [TypeId | Acc]).

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
decode_hashed_slot(Data) ->
    {Rest1, HasItem} = decode_bool(Data),
    case HasItem of
        false ->
            {Rest1, empty};
        true ->
            {ItemID,    Rest2} = decode_varint(Rest1),
            {ItemCount, Rest3} = decode_varint(Rest2),
            {NAdd,      Rest4} = decode_varint(Rest3),
            {Rest5, ComponentsToAdd}    = decode_hashed_slot_add_components(Rest4, NAdd, []),
            {NRemove,   Rest6}          = decode_varint(Rest5),
            {Rest7, ComponentsToRemove} = decode_slot_remove_components(Rest6, NRemove, []),
            {Rest7, {ItemID, ItemCount, ComponentsToAdd, ComponentsToRemove}}
    end.

decode_hashed_slot_add_components(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_hashed_slot_add_components(Data, N, Acc) ->
    {TypeId, Rest1} = decode_varint(Data),
    <<Hash:32/signed-integer, Rest2/binary>> = Rest1,
    decode_hashed_slot_add_components(Rest2, N - 1, [{TypeId, Hash} | Acc]).