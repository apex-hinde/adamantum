-module(decode).
-export([decode_type/2]).
-include("src/data_types/records.hrl").

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
        text_component ->
            decode_text_component(Data);
        json_text_component ->
            decode_json_text_component(Data);
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
            decode_byte_array(Data, Arg1);
        id_or_x ->
            decode_id_or_x(Data);
        {id_or_x, InnerType} ->
            decode_id_or_x(Data, InnerType);
	    id_set ->
	        decode_id_set(Data);
	    sound_event ->
	        decode_sound_event(Data);
	    teleport_flags ->
	        decode_teleport_flags(Data);
        recipe_display ->
            decode_recipe_display(Data);
        slot_display ->
            decode_slot_display(Data);
	    %%        light_data ->
	    %%            decode_light_data(Data);
        {either_x_or_y, TypeX, TypeY} ->
            decode_either_x_or_y(Data, TypeX, TypeY);
        game_profile ->
            decode_game_profile(Data);
        {resolvable_profile, BodyOption, CapeOption, ElytraOption, ModelOption} ->
            decode_resolvable_profile(Data, BodyOption, CapeOption, ElytraOption, ModelOption);
        debug_subscription_event ->
            decode_debug_subscription_event(Data);
        debug_subscription_update -> 
            decode_debug_subscription_update(Data);
        {debug_subscription_data, Type2} ->
            debug_subscription_data(Data, Type2);
        debug_path_node ->
            decode_debug_path_node(Data);
        debug_structure_info ->
            decode_debug_structure_info(Data);
        debug_structure_piece ->
            decode_debug_structure_piece(Data)
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
    {Data1, Length} = decode_varint(Data),
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
    {Rest, Val};

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
    {Data2, Length} = decode_varint(Data),
    <<Bit_set:(Length*8)/signed-integer, Data3/binary>> = Data2,
    {Data3, Bit_set}.
decode_fixed_bitset(Data) ->
    {Data2, Length} = decode_varint(Data),
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
            {RestData2, DecodedValue} = decode_type(RestData, InnerType),
            {RestData2, {some, DecodedValue}};
        false ->
            {RestData, none}
    end.

decode_id_or_x(Data) ->
    decode_id_or_x(Data, varint).

decode_id_or_x(Data, InnerType) ->
    case decode_varint(Data) of
        {error, Reason} ->
            {error, Reason};
        {Rest1, 0} ->
            case decode_type(Rest1, InnerType) of
                {error, Reason} ->
                    {error, Reason};
                {Rest2, Value} ->
                    {Rest2, {val, Value}}
            end;
        {Rest1, WireID} when is_integer(WireID), WireID > 0 ->
            {Rest1, {id, WireID - 1}};
        {_Rest1, WireID} when is_integer(WireID), WireID < 0 ->
            {error, "invalid id_or_x id"}
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
    {RestData, Count} = decode_varint(Data),
    decode_array(RestData, Count, ElemType).

decode_prefixed_array(Data, varint, ElemType) ->
    {RestData, Count} = decode_varint(Data),
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
    decode_varint(Data);

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
    {Rest1, ItemCount} = decode_varint(Data),
    case ItemCount of
        0 ->
            {Rest1, empty};
        _ ->
            {Rest2, ItemID}  = decode_varint(Rest1),
            {Rest3, NAdd}    = decode_varint(Rest2),
            {Rest4, NRemove} = decode_varint(Rest3),
            {Rest5, ComponentsToAdd}    = decode_slot_add_components(Rest4, NAdd, []),
            {Rest6, ComponentsToRemove} = decode_slot_remove_components(Rest5, NRemove, []),
            {Rest6, {ItemCount, ItemID, ComponentsToAdd, ComponentsToRemove}}
    end.

decode_slot_add_components(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_slot_add_components(Data, N, Acc) ->
    {Rest1, TypeId}  = decode_varint(Data),
    {Rest2, DataLen} = decode_varint(Rest1),
    <<DataBin:DataLen/binary, Rest3/binary>> = Rest2,
    decode_slot_add_components(Rest3, N - 1, [{TypeId, DataBin} | Acc]).

decode_slot_remove_components(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_slot_remove_components(Data, N, Acc) ->
    {Rest1, TypeId} = decode_varint(Data),
    decode_slot_remove_components(Rest1, N - 1, [TypeId | Acc]).


%% Hashed Slot
%%
%% Wire format (matches real Minecraft protocol for hashed slots):
%%   HasItem          :: Boolean
%%   [if HasItem]
%%     ItemID         :: VarInt
%%     ItemCount      :: VarInt
%%     NAdd           :: VarInt
%%     ComponentsToAdd    :: NAdd   × {TypeId::VarInt, HLoading /usr/lib/erlang/lib/tools-4.2/emacs/erlang-skels.el (source)...ash::Int32}
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
            {Rest2, ItemID}    = decode_varint(Rest1),
            {Rest3, ItemCount} = decode_varint(Rest2),
            {Rest4, NAdd}      = decode_varint(Rest3),
            {Rest5, ComponentsToAdd}    = decode_hashed_slot_add_components(Rest4, NAdd, []),
            {Rest6, NRemove}            = decode_varint(Rest5),
            {Rest7, ComponentsToRemove} = decode_slot_remove_components(Rest6, NRemove, []),
            {Rest7, {ItemID, ItemCount, ComponentsToAdd, ComponentsToRemove}}
    end.

decode_hashed_slot_add_components(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_hashed_slot_add_components(Data, N, Acc) ->
    {Rest1, TypeId} = decode_varint(Data),
    <<Hash:32/signed-integer, Rest2/binary>> = Rest1,
    decode_hashed_slot_add_components(Rest2, N - 1, [{TypeId, Hash} | Acc]).


decode_text_component(Data) when is_binary(Data) ->
    case Data of
        <<${, _/binary>> ->
            {<<>>, text_component:decode(Data)};
        <<$[, _/binary>> ->
            {<<>>, text_component:decode(Data)};
        <<$", _/binary>> ->
            {<<>>, text_component:decode(Data)};
        _ ->
            {Rest, Str} = decode_string(Data),
            {Rest, text_component:decode(Str)}
    end;
decode_text_component(Data) ->
    {<<>>, text_component:decode(Data)}.

decode_json_text_component(Data) when is_binary(Data) ->
    case Data of
        <<${, _/binary>> ->
            {<<>>, try json:decode(Data) catch _:_ -> Data end};
        <<$[, _/binary>> ->
            {<<>>, try json:decode(Data) catch _:_ -> Data end};
        <<$", _/binary>> ->
            {<<>>, try json:decode(Data) catch _:_ -> Data end};
        _ ->
            {Rest, Str} = decode_string(Data),
            Bin = list_to_binary(Str),
            Decoded = try json:decode(Bin) catch _:_ -> Bin end,
            {Rest, Decoded}
    end;
decode_json_text_component(Data) ->
    {<<>>, Data}.


decode_id_set(Data) ->
    case decode_varint(Data) of
        {error, Reason} ->
            {error, Reason};
        {Rest, 0} ->
            decode_string(Rest);
        {Rest, Val} when is_integer(Val), Val > 0 ->
            decode_varint_list(Rest, Val - 1, [])
    end.

decode_varint_list(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_varint_list(Data, Count, Acc) ->
    case decode_varint(Data) of
        {error, Reason} ->
            {error, Reason};
        {Rest, VarInt} ->
            decode_varint_list(Rest, Count - 1, [VarInt | Acc])
    end.


decode_sound_event(Data) ->
    {Data2, String} = decode_string(Data),
    {Data3, Bool} = decode_bool(Data2),
    case Bool of
        true -> 
            {Data4, Float} = decode_float(Data3),
            {Data4, {String, Bool, Float}};
        false -> {Data3, {String, Bool, undefined}}
    end.

decode_teleport_flags(Data) when is_binary(Data) ->
    {Rest, Int} = decode_int(Data),
    decode_teleport_flags_int(Rest, Int);
decode_teleport_flags(Int) when is_integer(Int) ->
    decode_teleport_flags_int(<<>>, Int).

decode_teleport_flags_int(Rest, Int) ->
    FlagsMap = #{
        relative_x => (Int band 16#0001) =/= 0,
        relative_y => (Int band 16#0002) =/= 0,
        relative_z => (Int band 16#0004) =/= 0,
        relative_yaw => (Int band 16#0008) =/= 0,
        relative_pitch => (Int band 16#0010) =/= 0,
        relative_velocity_x => (Int band 16#0020) =/= 0,
        relative_velocity_y => (Int band 16#0040) =/= 0,
        relative_velocity_z => (Int band 16#0080) =/= 0,
        rotate_velocity => (Int band 16#0100) =/= 0
    },
    {Rest, FlagsMap}.


decode_recipe_display(Data) ->
    {Data2, Type} = decode_varint(Data),
    case Type of
        0 -> decode_crafting_shapeless(Data2);
        1 -> decode_crafting_shaped(Data2);
        2 -> decode_furnace(Data2);
        3 -> decode_stonecutter(Data2);
        4 -> decode_smithing(Data2)
    end.


decode_crafting_shapeless(Data) ->
    {Data2, Count} = decode_varint(Data),
    {Data3, Ingredients} = decode_array(Data2, Count, slot_display),
    {Data4, Result} = decode_slot_display(Data3),
    {Data5, CraftingStation} = decode_slot_display(Data4),
    {Data5, #crafting_shapeless{type = 'minecraft:crafting_shapeless', ingredients_count = Count,
        ingredients = Ingredients, result = Result, crafting_station = CraftingStation}}.

decode_crafting_shaped(Data) ->
    {Data2, Width} = decode_varint(Data),
    {Data3, Height} = decode_varint(Data2),
    {Data4, Count} = decode_varint(Data3),
    {Data5, Ingredients} = decode_array(Data4, Count, slot_display),
    {Data6, Result} = decode_slot_display(Data5),
    {Data7, CraftingStation} = decode_slot_display(Data6),
    {Data7, #crafting_shaped{type = 'minecraft:crafting_shaped', width = Width, height = Height,
        ingredients_count = Count, ingredients = Ingredients, result = Result, crafting_station = CraftingStation}}.

decode_furnace(Data) ->
    {Data2, Ingredient} = decode_slot_display(Data),
    {Data3, Fuel} = decode_slot_display(Data2),
    {Data4, Result} = decode_slot_display(Data3),
    {Data5, CraftingStation} = decode_slot_display(Data4),
    {Data6, CookingTime} = decode_varint(Data5),
    {Data7, Experience} = decode_float(Data6),
    {Data7, #furnace{type = 'minecraft:furnace', ingredient = Ingredient, fuel = Fuel, result = Result,
        crafting_station = CraftingStation, cooking_time = CookingTime, experience = Experience}}.

decode_stonecutter(Data) ->
    {Data2, Ingredient} = decode_slot_display(Data),
    {Data3, Result} = decode_slot_display(Data2),
    {Data4, CraftingStation} = decode_slot_display(Data3),
    {Data4, #stonecutter{type = 'minecraft:stonecutter', ingredient = Ingredient, result = Result,
        crafting_station = CraftingStation}}.

decode_smithing(Data) ->
    {Data2, Template} = decode_slot_display(Data),
    {Data3, Base} = decode_slot_display(Data2),
    {Data4, Addition} = decode_slot_display(Data3),
    {Data5, Result} = decode_slot_display(Data4),
    {Data6, CraftingStation} = decode_slot_display(Data5),
    {Data6, #smithing{type = 'minecraft:smithing', template = Template, base = Base, addition = Addition,
        result = Result, crafting_station = CraftingStation}}.


decode_slot_display(Data) ->
    {Data2, Type} = decode_varint(Data),
    case Type of
        0 -> {Data2, #empty{type = 'minecraft:empty'}};
        1 -> {Data2, #any_fuel{type = 'minecraft:any_fuel'}};
        2 -> decode_with_any_potion(Data2);
        3 -> decode_only_with_component(Data2);
        4 -> decode_item(Data2);
        5 -> decode_item_stack(Data2);
        6 -> decode_tag(Data2);
        7 -> decode_dyed(Data2);
        8 -> decode_smithing_trim(Data2);
        9 -> decode_with_remainder(Data2);
        10 -> decode_composite(Data2)
    end.

decode_with_any_potion(Data) ->
    {Data2, Base} = decode_slot_display(Data),
    {Data2, #with_any_potion{type = 'minecraft:with_any_potion', base = Base}}.

decode_only_with_component(Data) ->
    {Data2, Base} = decode_slot_display(Data),
    {Data3, Component_Type_ID} = decode_varint(Data2),
    {Data3, #only_with_component{type = 'minecraft:only_with_component', base = Base, component_type_id = Component_Type_ID}}.

decode_item(Data) ->
    {Data2, ItemType} = decode_varint(Data),
    {Data2, #item{type = 'minecraft:item', item_type = ItemType}}.

decode_item_stack(Data) ->
    {Data2, ItemStack} = decode_slot(Data),
    {Data2, #item_stack{type = 'minecraft:item_stack', item_stack = ItemStack}}.

decode_tag(Data) ->
    {Data2, Tag} = decode_string(Data),
    {Data2, #tag{type = 'minecraft:tag', tag = Tag}}.

decode_dyed(Data) ->
    {Data2, Dye} = decode_slot_display(Data),
    {Data3, Target} = decode_slot_display(Data2),
    {Data3, #dyed{type = 'minecraft:dyed', dye = Dye, target = Target}}.

decode_smithing_trim(Data) ->
    {Data2, Base} = decode_slot_display(Data),
    {Data3, Material} = decode_slot_display(Data2),
    {Data4, Pattern} = decode_varint(Data3),
    {Data4, #smithing_trim{type='minecraft:smithing_trim', base = Base, material = Material, pattern = Pattern}}.

decode_with_remainder(Data) ->
    {Data2, Ingredient} = decode_slot_display(Data),
    {Data3, Remainder} = decode_slot_display(Data2),
    {Data3, #with_remainder{type='minecraft:with_remainder', ingredient = Ingredient, remainder = Remainder}}.

decode_composite(Data) ->
    {Data2, Option_Count} = decode_varint(Data),
    {Data3, Options} = decode_array(Data2, Option_Count, slot_display),
    {Data3, #composite{type = 'minecraft:composite', options_count = Option_Count, options = Options}}.



decode_either_x_or_y(Data, TypeX, TypeY) ->
    {Data2, Bool} = decode_bool(Data),
    case Bool of
        true -> decode_type(Data2, TypeX);
        false -> decode_type(Data2, TypeY)
    end.

decode_game_profile(Data) ->
    {Data2, UUID} = decode_uuid(Data),
    {Data3, Username} = decode_string(Data2),
    {Data4, Len} = decode_varint(Data3),
    {Data5, Properties} = decode_properties(Data4, Len),
    {Data5, {UUID, Username, Properties}}.

decode_properties(Data, Count) ->
    decode_properties_loop(Data, Count, []).

decode_properties_loop(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_properties_loop(Data, Count, Acc) ->
    {Data2, Name} = decode_string(Data),
    {Data3, Value} = decode_string(Data2),
    {Data4, Signature} = decode_prefixed_optional(Data3, string),
    decode_properties_loop(Data4, Count - 1, [{Name, Value, Signature} | Acc]).



decode_resolvable_profile(Data, BodyOption, CapeOption, ElytraOption, ModelOption) ->
    {Data2, ProfileKind} = decode_varint(Data),
    {Data3, Profile} = unpack_resolvable_profile(Data2, ProfileKind),
    {Data4, Body} = decode_optional(Data3, identifier, BodyOption),
    {Data5, Cape} = decode_optional(Data4, identifier, CapeOption),
    {Data6, Elytra} = decode_optional(Data5, identifier, ElytraOption),
    {Data7, Model} = decode_optional(Data6, varint, ModelOption),
    {Data7, {ProfileKind, Profile, Body, Cape, Elytra, Model}}.


unpack_resolvable_profile(Data, Profile_Kind) ->
    case Profile_Kind of
        0 ->
            {Data2, Username} = decode_prefixed_optional(Data, string),
            {Data3, UUID} = decode_prefixed_optional(Data2, uuid),
            {Data4, Len} = decode_varint(Data3),
            {Data5, Properties} = decode_properties(Data4, Len),
            {Data5, {Username, UUID, Properties}};
        1 ->
            decode_game_profile(Data)
    end.

decode_debug_subscription_update(Data) ->
    {Data2, Type} = decode_enum(Data),
    decode_prefixed_optional(Data2, {debug_subscription_data, Type}).

decode_debug_subscription_event(Data) ->
    {Data2, Type} = decode_enum(Data),
    debug_subscription_data(Data2, Type).

debug_subscription_data(Data, Type) ->
    case Type of
        0 -> {Data, #dedicated_server_tick_time{type = dedicated_server_tick_time}};
        1 -> bee(Data);
        2 -> villager_brain(Data);
        3 -> breeze(Data);
        4 -> goal_selector(Data);
        5 -> entity_path(Data);
        6 -> entity_block_intersection(Data);
        7 -> bee_hive(Data);
        8 -> poi(Data);
        9 -> redstone_wire_orientation(Data);
        10 -> {Data, #village_section{type = village_section}};
        11 -> raid(Data);
        12 -> structure(Data);
        13 -> game_event_listener(Data);
        14 -> neighbor_update(Data);
        15 -> game_event(Data)
    end.

bee(Data) ->
    {Data2, HivePosition} = decode_prefixed_optional(Data, position),
    {Data3, FlowerPosition} = decode_prefixed_optional(Data2, position),
    {Data4, TravelTicks} = decode_varint(Data3),
    {Data5, BlacklistedHives} = decode_prefixed_array(Data4, position),
    {Data5, #bee{type = bee, hive_position = HivePosition, flower_position = FlowerPosition,
        travel_ticks = TravelTicks, blacklisted_hives = BlacklistedHives}}.

villager_brain(Data) ->
    {Data2, Name} = decode_string(Data),
    {Data3, Profession} = decode_string(Data2),
    {Data4, XP} = decode_int(Data3),
    {Data5, Health} = decode_float(Data4),
    {Data6, MaxHealth} = decode_float(Data5),
    {Data7, Inventory} = decode_string(Data6),
    {Data8, WantsGolem} = decode_bool(Data7),
    {Data9, AngerLevel} = decode_int(Data8),
    {Data10, Activities} = decode_prefixed_array(Data9, string),
    {Data11, Behaviors} = decode_prefixed_array(Data10, string),
    {Data12, Memories} = decode_prefixed_array(Data11, string),
    {Data13, Gossips} = decode_prefixed_array(Data12, string),
    {Data14, POIs} = decode_prefixed_array(Data13, position),
    {Data15, PotentialPOIs} = decode_prefixed_array(Data14, position),
    {Data15, #villager_brain{type = villager_brain, name = Name, profession = Profession,
        xp = XP, health = Health, max_health = MaxHealth, inventory = Inventory,
        wants_golem = WantsGolem, anger_level = AngerLevel, activities = Activities,
        behaviors = Behaviors, memories = Memories, gossips = Gossips, pois = POIs,
        potential_pois = PotentialPOIs}}.

breeze(Data) ->
    {Data2, AttackTarget} = decode_prefixed_optional(Data, varint),
    {Data3, JumpTarget} = decode_prefixed_optional(Data2, position),
    {Data3, #breeze{type = breeze, attack_target = AttackTarget, jump_target = JumpTarget}}.

goal_selector(Data) ->
    {Data2, Priority} = decode_varint(Data),
    {Data3, IsRunning} = decode_bool(Data2),
    {Data4, Name} = decode_string(Data3),
    {Data4, #goal_selector{type = goal_selector, priority = Priority, is_running = IsRunning, name = Name}}.

entity_path(Data) ->
    {Data2, Reached} = decode_bool(Data),
    {Data3, NextBlockIndex} = decode_int(Data2),
    {Data4, BlockPosition} = decode_position(Data3),
    {Data5, Nodes} = decode_prefixed_array(Data4, debug_path_node),
    {Data6, TargetNodes} = decode_prefixed_array(Data5, debug_path_node),
    {Data7, OpenSet} = decode_prefixed_array(Data6, debug_path_node),
    {Data8, ClosedSet} = decode_prefixed_array(Data7, debug_path_node),
    {Data9, MaxNodeDistance} = decode_float(Data8),
    {Data9, #entity_path{type = entity_path, reached = Reached, next_block_index = NextBlockIndex,
        block_position = BlockPosition, nodes = Nodes, target_nodes = TargetNodes,
        open_set = OpenSet, closed_set = ClosedSet, max_node_distance = MaxNodeDistance}}.

decode_debug_path_node(Data) ->
    {Data2, X} = decode_int(Data),
    {Data3, Y} = decode_int(Data2),
    {Data4, Z} = decode_int(Data3),
    {Data5, WalkCost} = decode_float(Data4),
    {Data6, Penalty} = decode_float(Data5),
    {Data7, Open} = decode_bool(Data6),
    {Data8, Type} = decode_varint(Data7),
    {Data9, HeapIndex} = decode_int(Data8),
    {Data9, {X, Y, Z, WalkCost, Penalty, Open, Type, HeapIndex}}.

entity_block_intersection(Data) ->
    {Data2, ID} = decode_enum(Data),
    {Data2, #entity_block_intersection{type = entity_block_intersection, id = ID}}.

bee_hive(Data) ->
    {Data2, HiveType} = decode_enum(Data),
    {Data3, OccupantCount} = decode_varint(Data2),
    {Data4, HoneyLevel} = decode_varint(Data3),
    {Data5, Sedated} = decode_bool(Data4),
    {Data5, #bee_hive{type = bee_hive, hive_type = HiveType, occupant_count = OccupantCount,
        honey_level = HoneyLevel, sedated = Sedated}}.

poi(Data) ->
    {Data2, Position} = decode_position(Data),
    {Data3, POIType} = decode_enum(Data2),
    {Data4, FreeTicketCount} = decode_varint(Data3),
    {Data4, #poi{type = poi, position = Position, poi_type = POIType, free_ticket_count = FreeTicketCount}}.

redstone_wire_orientation(Data) ->
    {Data2, ID} = decode_varint(Data),
    {Data2, #redstone_wire_orientation{type = redstone_wire_orientation, id = ID}}.

raid(Data) ->
    {Data2, Positions} = decode_prefixed_array(Data, position),
    {Data2, #raid{type = raid, positions = Positions}}.

structure(Data) ->
    {Data2, Structures} = decode_prefixed_array(Data, debug_structure_info),
    {Data2, #structure{type = structure, structures = Structures}}.

decode_debug_structure_info(Data) ->
    {Data2, MinX} = decode_int(Data),
    {Data3, MinY} = decode_int(Data2),
    {Data4, MinZ} = decode_int(Data3),
    {Data5, MaxX} = decode_int(Data4),
    {Data6, MaxY} = decode_int(Data5),
    {Data7, MaxZ} = decode_int(Data6),
    {Data8, Pieces} = decode_prefixed_array(Data7, debug_structure_piece),
    {Data8, {{MinX, MinY, MinZ, MaxX, MaxY, MaxZ}, Pieces}}.

decode_debug_structure_piece(Data) ->
    {Data2, MinX} = decode_int(Data),
    {Data3, MinY} = decode_int(Data2),
    {Data4, MinZ} = decode_int(Data3),
    {Data5, MaxX} = decode_int(Data4),
    {Data6, MaxY} = decode_int(Data5),
    {Data7, MaxZ} = decode_int(Data6),
    {Data8, IsStart} = decode_bool(Data7),
    {Data8, {{MinX, MinY, MinZ, MaxX, MaxY, MaxZ}, IsStart}}.

game_event_listener(Data) ->
    {Data2, ListenerRadius} = decode_varint(Data),
    {Data2, #game_event_listener{type = game_event_listener, listener_radius = ListenerRadius}}.

neighbor_update(Data) ->
    {Data2, Position} = decode_position(Data),
    {Data2, #neighbor_update{type = neighbor_update, position = Position}}.

game_event(Data) ->
    {Data2, Event} = decode_enum(Data),
    {Data3, X} = decode_double(Data2),
    {Data4, Y} = decode_double(Data3),
    {Data5, Z} = decode_double(Data4),
    {Data5, #game_event{type = game_event, event = Event, x = X, y = Y, z = Z}}.
