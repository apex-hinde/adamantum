-module(decode).
-export([decode_type/2, decode_message/2, extract_value/1]).
-include("src/data_types/records.hrl").

decode_message(Data, Packet_name) ->
    {_, Param_list} = data_packets:get_messages_serverbound(Packet_name),
    Data2 = decode_message_list(Data, Param_list,  []),
    Return = msg_to_record:msg_to_record({Packet_name, Data2}),
    io:format("~p~n", [Return]),
    Return.

decode_message_list(<<>>, [], Acc) ->
    lists:reverse(Acc);

decode_message_list(Data, [], Acc) ->
    {Data, Acc};
decode_message_list(Data, [H|T],  Acc) ->
    {Data2, Result} = decode_type(Data, H),
    decode_message_list(Data2, T, [Result|Acc]).



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
            decode_identifier(Data);
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
            decode_nbt(Data);
	    position ->
            decode_position(Data);
        angle ->
            decode_angle(Data);
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
        {prefixed_optional, Inner_Type} ->
            decode_prefixed_optional(Data, Inner_Type);
        {array, Arg1, Arg2} ->
            decode_array(Data, Arg1, Arg2);
        {prefixed_array, ElemList} when is_list(ElemList) ->
            decode_prefixed_array_list(Data, ElemList);
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
            decode_debug_structure_piece(Data);
        lp_vec3 ->
            decode_lp_vec3(Data);
        seen_advancements ->
            decode_seen_advancements(Data);
        boss_bar ->
            decode_boss_bar(Data);
        node ->
            decode_node(Data);
        delete_chat ->
            decode_delete_chat(Data);
        chat_type ->
            decode_chat_type(Data);
        player_info_update ->
            decode_player_info_update(Data);
        set_equipment ->
            decode_set_equipment(Data);
        set_objective ->
            decode_set_objective(Data);
        set_player_team ->
            decode_set_player_team(Data);
        waypoint_data ->
            decode_waypoint_data(Data);
        stop_sound ->
            decode_stop_sound(Data);
        set_score ->
            decode_set_score(Data);
        update_advancements ->
            decode_update_advancements(Data);
        advancement ->
            decode_advancement(Data);
        advancement_progress ->
            decode_advancement_progress(Data);

        %%Component Data Types
        _ -> 
            component_decode:decode_component(Data, Type)
    end.

extract_value(#bool{bool = V}) -> V;
extract_value(#byte{byte = V}) -> V;
extract_value(#ubyte{ubyte = V}) -> V;
extract_value(#short{short = V}) -> V;
extract_value(#ushort{ushort = V}) -> V;
extract_value(#int{int = V}) -> V;
extract_value(#long{long = V}) -> V;
extract_value(#float{float = V}) -> V;
extract_value(#double{double = V}) -> V;
extract_value(#uuid{uuid = V}) -> V;
extract_value(#string{string = V}) -> V;
extract_value(#identifier{identifier = V}) -> extract_value(V);
extract_value(#varint{varint = V}) -> V;
extract_value(#varlong{varlong = V}) -> V;
extract_value(#enum{enum = V}) -> extract_value(V);
extract_value(#optional{optional = V}) -> extract_value(V);
extract_value(#byte_array{byte_array = V}) -> V;
extract_value(V) when not is_tuple(V) -> V;

extract_value(V) -> V.

decode_nbt(Data) ->
    case nbt:decode(Data) of
        {Data2, Map} -> {Data2, #nbt{nbt = Map}};
        Map when is_list(Map) -> {<<>>, #nbt{nbt = Map}}
    end.

decode_bool(Data) ->
    <<Bool:8, Data2/binary>> = Data,
    {Data2, #bool{bool = Bool =:= 1}}.

decode_byte(Data) ->
    <<Byte:8/signed-integer, Data2/binary>> = Data,
    {Data2, #byte{byte = Byte}}.

decode_ubyte(Data) ->
    <<UByte:8/unsigned-integer, Data2/binary>> = Data,
    {Data2, #ubyte{ubyte = UByte}}.

decode_short(Data) ->
    <<Short:16/signed-integer, Data2/binary>> = Data,
    {Data2, #short{short = Short}}.

decode_ushort(Data) ->
    <<UShort:16/unsigned-integer, Data2/binary>> = Data,
    {Data2, #ushort{ushort = UShort}}.

decode_int(Data) ->
    <<Int:32/signed-integer, Data2/binary>> = Data,
    {Data2, #int{int = Int}}.

decode_long(Data) ->
    <<Long:64/signed-integer, Data2/binary>> = Data,
    {Data2, #long{long = Long}}.

decode_float(Data) ->
    <<Float:32/float, Data2/binary>> = Data,
    {Data2, #float{float = Float}}.

decode_double(Data) ->
    <<Double:64/float, Data2/binary>> = Data,
    {Data2, #double{double = Double}}.

decode_string(Data) ->
    {Data1, #varint{varint = Length}} = decode_varint(Data),
    <<String:Length/binary, Data2/binary>> = Data1,
    String2 = binary_to_list(String),
    {Data2, #string{string = String2}}.

decode_identifier(Data) ->
    {Data1, #string{string = String}} = decode_string(Data),
    {Data1, #identifier{identifier = String}}.

decode_angle(Data) ->
    <<Angle:8/signed-integer, Data2/binary>> = Data,
    {Data2, #angle{angle = Angle}}.

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
    {Rest, #varint{varint = Val}};
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
    {Rest, #varlong{varlong = Val}};
decode_varlong(<<>>, _, _) ->
    {error, "insufficient data"}.

decode_position(Data) ->
    <<X:26/signed-integer, Z:26/signed-integer, Y:12/signed-integer, Data2/binary>> = Data,
    {Data2, #position{x = X, y = Y, z = Z}}.

decode_uuid(Data) ->
    <<UUID:128/bitstring, Data2/binary>> = Data,
    {Data2, #uuid{uuid = UUID}}.

decode_bitset(Data) ->
    {Data2, #varint{varint = Length}} = decode_varint(Data),
    <<Bit_set:(Length*8)/signed-integer, Data3/binary>> = Data2,
    {Data3, #bitset{bitset = Bit_set}}.

decode_fixed_bitset(Data) ->
    {Data2, #varint{varint = Length}} = decode_varint(Data),
    <<Bit_set:Length/signed-integer, Data3/binary>> = Data2,
    {Data3, #fixed_bitset{fixed_bitset = Bit_set}}.

decode_byte_array(Data) ->
    {<<>>, #byte_array{byte_array = Data}}.

decode_byte_array(Data, Length) when is_integer(Length) ->
    <<ByteArray:Length/binary, Data2/binary>> = Data,
    {Data2, #byte_array{byte_array = ByteArray}};
decode_byte_array(Data, PrefixType) when is_atom(PrefixType) ->
    {RestData, PrefixRec} = decode_type_internal(Data, PrefixType),
    Length = extract_value(PrefixRec),
    <<ByteArray:Length/binary, Data2/binary>> = RestData,
    {Data2, #byte_array{byte_array = ByteArray}}.

decode_optional(Data, Types, true) when is_list(Types) ->
    {DecodedFields, RestData} = lists:mapfoldl(
        fun(ElemType, AccData) ->
            {NewData, Elem} = decode_type(AccData, ElemType),
            {Elem, NewData}
        end,
        Data,
        Types
    ),
    {RestData, #optional{some = some, optional = list_to_tuple(DecodedFields)}};
decode_optional(Data, Type, true) ->
    {RestData, Value} = decode_type(Data, Type),
    {RestData, #optional{some = some, optional = Value}};
decode_optional(Data, _Type, false) ->
    {Data, #optional{some = none, optional = none}}.

decode_prefixed_optional(Data, InnerTypes) when is_list(InnerTypes) ->
    {RestData, #bool{bool = IsPresent}} = decode_bool(Data),
    case IsPresent of
        true ->
            {DecodedFields, RestData2} = lists:mapfoldl(
                fun(ElemType, AccData) ->
                    {NewData, Elem} = decode_type(AccData, ElemType),
                    {Elem, NewData}
                end,
                RestData,
                InnerTypes
            ),
            {RestData2, #prefixed_optional{some = some, prefixed_optional = list_to_tuple(DecodedFields)}};
        false ->
            {RestData, #prefixed_optional{some = none, prefixed_optional = none}}
    end;
decode_prefixed_optional(Data, InnerType) ->
    {RestData, #bool{bool = IsPresent}} = decode_bool(Data),
    case IsPresent of
        true ->
            {RestData2, DecodedValue} = decode_type(RestData, InnerType),
            {RestData2, #prefixed_optional{some = some, prefixed_optional = DecodedValue}};
        false ->
            {RestData, #prefixed_optional{some = none, prefixed_optional = none}}
    end.

decode_id_or_x(Data) ->
    decode_id_or_x(Data, varint).

decode_id_or_x(Data, InnerType) ->
    case decode_varint(Data) of
        {error, Reason} ->
            {error, Reason};
        {Rest1, #varint{varint = 0}} ->
            case decode_type(Rest1, InnerType) of
                {error, Reason} ->
                    {error, Reason};
                {Rest2, Value} ->
                    {Rest2, #id_or_x{id_or_x = Value}}
            end;
        {Rest1, #varint{varint = WireID}} when is_integer(WireID), WireID > 0 ->
            {Rest1, #id_or_x{id_or_x = WireID - 1}};
        {_Rest1, #varint{varint = WireID}} when is_integer(WireID), WireID < 0 ->
            {error, "invalid id_or_x id"}
    end.

decode_array(Data, Count, ElemList) when is_integer(Count), Count >= 0, is_list(ElemList) ->
    decode_array_list(Data, Count, ElemList);
decode_array(Data, ElemList, Count) when is_integer(Count), Count >= 0, is_list(ElemList) ->
    decode_array_list(Data, Count, ElemList);
decode_array(Data, Count, ElemType) when is_integer(Count), Count >= 0 ->
    decode_array_loop(Data, Count, ElemType, []);
decode_array(Data, ElemType, Count) when is_integer(Count), Count >= 0 ->
    decode_array_loop(Data, Count, ElemType, []).

decode_array_loop(Data, 0, _ElemType, Acc) ->
    {Data, #array{array = lists:reverse(Acc)}};
decode_array_loop(Data, Count, ElemType, Acc) ->
    {RestData, Elem} = decode_type(Data, ElemType),
    decode_array_loop(RestData, Count - 1, ElemType, [Elem | Acc]).

decode_prefixed_array(Data, ElemType) ->
    {RestData, #varint{varint = Count}} = decode_varint(Data),
    {RestData2, #array{array = Array}} = decode_array(RestData, Count, ElemType),
    {RestData2, #prefixed_array{prefixed_array = Array}}.

decode_prefixed_array(Data, varint, ElemType) ->
    decode_prefixed_array(Data, ElemType);
decode_prefixed_array(Data, PrefixType, ElemType) ->
    {RestData, PrefixRec} = decode_type(Data, PrefixType),
    Count = extract_value(PrefixRec),
    {RestData2, #array{array = Array}} = decode_array(RestData, Count, ElemType),
    {RestData2, #prefixed_array{prefixed_array = Array}}.

decode_array_list(Data, Count, ElemList) when is_integer(Count), Count >= 0, is_list(ElemList) ->
    decode_array_loop_list(Data, Count, ElemList, []).

decode_array_loop_list(Data, 0, _ElemList, Acc) ->
    {Data, #array{array = lists:reverse(Acc)}};
decode_array_loop_list(Data, Count, ElemList, Acc) ->
    {DecodedFields, RestData} = lists:mapfoldl(
        fun(ElemType, AccData) ->
            io:format("elem type~p~n", [ElemType]),
            {NewData, Elem} = decode_type(AccData, ElemType),
            io:format("new data~p~n", [NewData]),

            {Elem, NewData}
        end,
        Data,
        ElemList
    ),
    decode_array_loop_list(RestData, Count - 1, ElemList, [list_to_tuple(DecodedFields) | Acc]).

decode_prefixed_array_list(Data, ElemList) ->
    decode_prefixed_array_list(Data, varint, ElemList).

decode_prefixed_array_list(Data, PrefixType, ElemList) ->
    {RestData, PrefixRec} = decode_type(Data, PrefixType),
    Count = extract_value(PrefixRec),
    {RestData2, #array{array = Array}} = decode_array_list(RestData, Count, ElemList),
    {RestData2, #prefixed_array{prefixed_array = Array}}.

decode_enum(Data) ->
    decode_enum(Data, varint).

decode_enum(Data, InnerType) when is_atom(InnerType) ->
    case decode_type_internal(Data, InnerType) of
        {error, Reason} -> {error, Reason};
        {Rest, Rec} when is_tuple(Rec), element(1, Rec) =:= enum -> {Rest, Rec};
        {Rest, Rec} -> {Rest, #enum{enum = extract_value(Rec)}}
    end;
decode_enum(Data, EnumList) when is_list(EnumList); is_map(EnumList) ->
    decode_enum(Data, varint, EnumList).

decode_enum(Data, InnerType, EnumList) ->
    case decode_type_internal(Data, InnerType) of
        {error, Reason} ->
            {error, Reason};
        {Rest, Rec} when is_list(EnumList) ->
            Val = extract_value(Rec),
            if is_integer(Val) andalso Val >= 0 andalso Val < length(EnumList) ->
                    {Rest, #enum{enum = lists:nth(Val + 1, EnumList)}};
               true ->
                    case lists:member(Val, EnumList) of
                        true -> {Rest, #enum{enum = Val}};
                        false -> {error, "invalid enum value"}
                    end
            end;
        {Rest, Rec} when is_map(EnumList) ->
            Val = extract_value(Rec),
            case EnumList of
		#{Val := MappedVal} ->
		    {Rest, #enum{enum = MappedVal}};
		#{} ->
		    case lists:member(Val, maps:values(EnumList)) of
                        true -> {Rest, #enum{enum = Val}};
                        false -> {error, "invalid enum value"}
                    end
	    end
    end.

decode_type_internal(Data, varint) ->
    decode_varint(Data);
decode_type_internal(Data, varlong) ->
    decode_varlong(Data);
decode_type_internal(Data, InnerType) ->
    decode_type(Data, InnerType).

decode_slot(Data) ->
    {Rest1, #varint{varint = ItemCount}} = decode_varint(Data),
    case ItemCount of
        0 ->
            {Rest1, #slot{item_count = 0, itemID = undefined, components_to_add = [], components_to_remove = []}};
        _ ->
            {Rest2, #varint{varint = ItemID}}  = decode_varint(Rest1),
            {Rest3, #varint{varint = NAdd}}    = decode_varint(Rest2),
            {Rest4, #varint{varint = NRemove}} = decode_varint(Rest3),
            {Rest5, ComponentsToAdd}    = decode_slot_add_components(Rest4, NAdd, []),
            {Rest6, ComponentsToRemove} = decode_slot_remove_components(Rest5, NRemove, []),
            {Rest6, #slot{item_count = ItemCount, itemID = ItemID, components_to_add = ComponentsToAdd, components_to_remove = ComponentsToRemove}}
    end.

decode_slot_add_components(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_slot_add_components(Data, N, Acc) ->
    {Rest1, #varint{varint = TypeId}} = decode_varint(Data),
    {Rest2, ComponentData} = component_decode:decode_component(TypeId, Rest1),
    decode_slot_add_components(Rest2, N - 1, [{TypeId, ComponentData} | Acc]).


decode_slot_remove_components(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_slot_remove_components(Data, N, Acc) ->
    {Rest1, #varint{varint = TypeId}} = decode_varint(Data),
    decode_slot_remove_components(Rest1, N - 1, [TypeId | Acc]).

decode_hashed_slot(Data) ->
    {Rest1, #bool{bool = HasItem}} = decode_bool(Data),
    case HasItem of
        false ->
            {Rest1, #hashed_slot{item_count = 0, itemID = undefined, components_to_add = [], components_to_remove = []}};
        true ->
            {Rest2, #varint{varint = ItemID}}    = decode_varint(Rest1),
            {Rest3, #varint{varint = ItemCount}} = decode_varint(Rest2),
            {Rest4, #varint{varint = NAdd}}      = decode_varint(Rest3),
            {Rest5, ComponentsToAdd}    = decode_hashed_slot_add_components(Rest4, NAdd, []),
            {Rest6, #varint{varint = NRemove}}   = decode_varint(Rest5),
            {Rest7, ComponentsToRemove} = decode_slot_remove_components(Rest6, NRemove, []),
            {Rest7, #hashed_slot{item_count = ItemCount, itemID = ItemID, components_to_add = ComponentsToAdd, components_to_remove = ComponentsToRemove}}
    end.

decode_hashed_slot_add_components(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_hashed_slot_add_components(Data, N, Acc) ->
    {Rest1, #varint{varint = TypeId}} = decode_varint(Data),
    <<Hash:32/signed-integer, Rest2/binary>> = Rest1,
    decode_hashed_slot_add_components(Rest2, N - 1, [{TypeId, Hash} | Acc]).

decode_text_component(Data) when is_binary(Data) ->
    case decode_string(Data) of
        {Rest, #string{string = Str}} ->
            {Rest, #text_component{component_map = text_component:decode(Str)}};
        _ ->
            {<<>>, #text_component{component_map = text_component:decode(Data)}}
    end;
decode_text_component(Data) ->
    {<<>>, #text_component{component_map = text_component:decode(Data)}}.

decode_json_text_component(Data) when is_binary(Data) ->
    case Data of
        <<${, _/binary>> ->
            {<<>>, #json_text_component{json_component_map = try json:decode(Data) catch _:_ -> Data end}};
        <<$[, _/binary>> ->
            {<<>>, #json_text_component{json_component_map = try json:decode(Data) catch _:_ -> Data end}};
        <<$", _/binary>> ->
            {<<>>, #json_text_component{json_component_map = try json:decode(Data) catch _:_ -> Data end}};
        _ ->
            {Rest, #string{string = Str}} = decode_string(Data),
            Bin = list_to_binary(Str),
            Decoded = try json:decode(Bin) catch _:_ -> Bin end,
            {Rest, #json_text_component{json_component_map = Decoded}}
    end;
decode_json_text_component(Data) ->
    {<<>>, #json_text_component{json_component_map = Data}}.

decode_id_set(Data) ->
    case decode_varint(Data) of
        {error, Reason} ->
            {error, Reason};
        {Rest, #varint{varint = 0}} ->
            {Rest2, #string{string = Tag}} = decode_string(Rest),
            {Rest2, #id_set{id_set = Tag}};
        {Rest, #varint{varint = Val}} when is_integer(Val), Val > 0 ->
            {Rest2, List} = decode_varint_list(Rest, Val - 1, []),
            {Rest2, #id_set{id_set = List}}
    end.

decode_varint_list(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_varint_list(Data, Count, Acc) ->
    case decode_varint(Data) of
        {error, Reason} ->
            {error, Reason};
        {Rest, #varint{varint = VarInt}} ->
            decode_varint_list(Rest, Count - 1, [VarInt | Acc])
    end.

decode_sound_event(Data) ->
    {Data2, #string{string = String}} = decode_string(Data),
    {Data3, #bool{bool = Bool}} = decode_bool(Data2),
    case Bool of
        true -> 
            {Data4, #float{float = Float}} = decode_float(Data3),
            {Data4, #sound_event{sound_name = String, has_fixed_value = Bool, fixed_range = Float}};
        false -> {Data3, #sound_event{sound_name = String, has_fixed_value = Bool, fixed_range = undefined}}
    end.

decode_teleport_flags(Data) when is_binary(Data) ->
    {Rest, #int{int = Int}} = decode_int(Data),
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
    {Rest, #teleport_flags{flagsmap = FlagsMap}}.

decode_recipe_display(Data) ->
    {Data2, #varint{varint = Type}} = decode_varint(Data),
    case Type of
        0 -> decode_crafting_shapeless(Data2);
        1 -> decode_crafting_shaped(Data2);
        2 -> decode_furnace(Data2);
        3 -> decode_stonecutter(Data2);
        4 -> decode_smithing(Data2)
    end.

decode_crafting_shapeless(Data) ->
    {Data2, #varint{varint = Count}} = decode_varint(Data),
    {Data3, #array{array = Ingredients}} = decode_array(Data2, Count, slot_display),
    {Data4, Result} = decode_slot_display(Data3),
    {Data5, CraftingStation} = decode_slot_display(Data4),
    {Data5, #crafting_shapeless{type = 'minecraft:crafting_shapeless', ingredients_count = Count,
        ingredients = Ingredients, result = Result, crafting_station = CraftingStation}}.

decode_crafting_shaped(Data) ->
    {Data2, #varint{varint = Width}} = decode_varint(Data),
    {Data3, #varint{varint = Height}} = decode_varint(Data2),
    {Data4, #varint{varint = Count}} = decode_varint(Data3),
    {Data5, #array{array = Ingredients}} = decode_array(Data4, Count, slot_display),
    {Data6, Result} = decode_slot_display(Data5),
    {Data7, CraftingStation} = decode_slot_display(Data6),
    {Data7, #crafting_shaped{type = 'minecraft:crafting_shaped', width = Width, height = Height,
        ingredients_count = Count, ingredients = Ingredients, result = Result, crafting_station = CraftingStation}}.

decode_furnace(Data) ->
    {Data2, Ingredient} = decode_slot_display(Data),
    {Data3, Fuel} = decode_slot_display(Data2),
    {Data4, Result} = decode_slot_display(Data3),
    {Data5, CraftingStation} = decode_slot_display(Data4),
    {Data6, #varint{varint = CookingTime}} = decode_varint(Data5),
    {Data7, #float{float = Experience}} = decode_float(Data6),
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
    {Data2, #varint{varint = Type}} = decode_varint(Data),
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
    {Data3, #varint{varint = Component_Type_ID}} = decode_varint(Data2),
    {Data3, #only_with_component{type = 'minecraft:only_with_component', base = Base, component_type_id = Component_Type_ID}}.

decode_item(Data) ->
    {Data2, #varint{varint = ItemType}} = decode_varint(Data),
    {Data2, #item{type = 'minecraft:item', item_type = ItemType}}.

decode_item_stack(Data) ->
    {Data2, ItemStack} = decode_slot(Data),
    {Data2, #item_stack{type = 'minecraft:item_stack', item_stack = ItemStack}}.

decode_tag(Data) ->
    {Data2, #string{string = Tag}} = decode_string(Data),
    {Data2, #tag{type = 'minecraft:tag', tag = Tag}}.

decode_dyed(Data) ->
    {Data2, Dye} = decode_slot_display(Data),
    {Data3, Target} = decode_slot_display(Data2),
    {Data3, #dyed{type = 'minecraft:dyed', dye = Dye, target = Target}}.

decode_smithing_trim(Data) ->
    {Data2, Base} = decode_slot_display(Data),
    {Data3, Material} = decode_slot_display(Data2),
    {Data4, #varint{varint = Pattern}} = decode_varint(Data3),
    {Data4, #smithing_trim{type='minecraft:smithing_trim', base = Base, material = Material, pattern = Pattern}}.

decode_with_remainder(Data) ->
    {Data2, Ingredient} = decode_slot_display(Data),
    {Data3, Remainder} = decode_slot_display(Data2),
    {Data3, #with_remainder{type='minecraft:with_remainder', ingredient = Ingredient, remainder = Remainder}}.

decode_composite(Data) ->
    {Data2, #varint{varint = Option_Count}} = decode_varint(Data),
    {Data3, #array{array = Options}} = decode_array(Data2, Option_Count, slot_display),
    {Data3, #composite{type = 'minecraft:composite', options_count = Option_Count, options = Options}}.

decode_either_x_or_y(Data, TypeX, TypeY) ->
    {Data2, #bool{bool = Bool}} = decode_bool(Data),
    case Bool of
        true ->
            {Data3, ValX} = decode_type(Data2, TypeX),
            {Data3, #either_x_or_y{x = ValX, y = undefined}};
        false ->
            {Data3, ValY} = decode_type(Data2, TypeY),
            {Data3, #either_x_or_y{x = undefined, y = ValY}}
    end.

decode_game_profile(Data) ->
    {Data2, #uuid{uuid = UUID}} = decode_uuid(Data),
    {Data3, #string{string = Username}} = decode_string(Data2),
    {Data4, #varint{varint = Len}} = decode_varint(Data3),
    {Data5, Properties} = decode_properties(Data4, Len),
    {Data5, #game_profile{uuid = UUID, username = Username, properties = Properties}}.

decode_properties(Data, Count) ->
    decode_properties_loop(Data, Count, []).

decode_properties_loop(Data, 0, Acc) ->
    {Data, lists:reverse(Acc)};
decode_properties_loop(Data, Count, Acc) ->
    {Data2, #string{string = Name}} = decode_string(Data),
    {Data3, #string{string = Value}} = decode_string(Data2),
    {Data4, Signature} = decode_prefixed_optional(Data3, string),
    decode_properties_loop(Data4, Count - 1, [{Name, Value, Signature} | Acc]).

decode_resolvable_profile(Data, BodyOption, CapeOption, ElytraOption, ModelOption) ->
    {Data2, #varint{varint = ProfileKind}} = decode_varint(Data),
    {Data3, Profile} = unpack_resolvable_profile(Data2, ProfileKind),
    {Data4, Body} = decode_optional(Data3, identifier, BodyOption),
    {Data5, Cape} = decode_optional(Data4, identifier, CapeOption),
    {Data6, Elytra} = decode_optional(Data5, identifier, ElytraOption),
    {Data7, Model} = decode_optional(Data6, varint, ModelOption),
    {Data7, #resolvable_profile{profile_kind = ProfileKind, profile = Profile, body = Body, cape = Cape, elytra = Elytra, model = Model}}.

unpack_resolvable_profile(Data, Profile_Kind) ->
    case Profile_Kind of
        0 ->
            {Data2, Username} = decode_prefixed_optional(Data, string),
            {Data3, UUID} = decode_prefixed_optional(Data2, uuid),
            {Data4, #varint{varint = Len}} = decode_varint(Data3),
            {Data5, Properties} = decode_properties(Data4, Len),
            {Data5, {Username, UUID, Properties}};
        1 ->
            decode_game_profile(Data)
    end.

decode_debug_subscription_update(Data) ->
    {Data2, EnumRec} = decode_enum(Data),
    Type = extract_value(EnumRec),
    decode_prefixed_optional(Data2, {debug_subscription_data, Type}).

decode_debug_subscription_event(Data) ->
    {Data2, EnumRec} = decode_enum(Data),
    Type = extract_value(EnumRec),
    {Data3, SubData} = debug_subscription_data(Data2, Type),
    {Data3, #debug_subscription_event{debug_subscription_type = Type, data = SubData}}.

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
    {Data4, #varint{varint = TravelTicks}} = decode_varint(Data3),
    {Data5, #prefixed_array{prefixed_array = BlacklistedHives}} = decode_prefixed_array(Data4, position),
    {Data5, #bee{type = bee, hive_position = HivePosition, flower_position = FlowerPosition,
        travel_ticks = TravelTicks, blacklisted_hives = BlacklistedHives}}.

villager_brain(Data) ->
    {Data2, #string{string = Name}} = decode_string(Data),
    {Data3, #string{string = Profession}} = decode_string(Data2),
    {Data4, #int{int = XP}} = decode_int(Data3),
    {Data5, #float{float = Health}} = decode_float(Data4),
    {Data6, #float{float = MaxHealth}} = decode_float(Data5),
    {Data7, #string{string = Inventory}} = decode_string(Data6),
    {Data8, #bool{bool = WantsGolem}} = decode_bool(Data7),
    {Data9, #int{int = AngerLevel}} = decode_int(Data8),
    {Data10, #prefixed_array{prefixed_array = Activities}} = decode_prefixed_array(Data9, string),
    {Data11, #prefixed_array{prefixed_array = Behaviors}} = decode_prefixed_array(Data10, string),
    {Data12, #prefixed_array{prefixed_array = Memories}} = decode_prefixed_array(Data11, string),
    {Data13, #prefixed_array{prefixed_array = Gossips}} = decode_prefixed_array(Data12, string),
    {Data14, #prefixed_array{prefixed_array = POIs}} = decode_prefixed_array(Data13, position),
    {Data15, #prefixed_array{prefixed_array = PotentialPOIs}} = decode_prefixed_array(Data14, position),
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
    {Data2, #varint{varint = Priority}} = decode_varint(Data),
    {Data3, #bool{bool = IsRunning}} = decode_bool(Data2),
    {Data4, #string{string = Name}} = decode_string(Data3),
    {Data4, #goal_selector{type = goal_selector, priority = Priority, is_running = IsRunning, name = Name}}.

entity_path(Data) ->
    {Data2, #bool{bool = Reached}} = decode_bool(Data),
    {Data3, #int{int = NextBlockIndex}} = decode_int(Data2),
    {Data4, BlockPosition} = decode_position(Data3),
    {Data5, #prefixed_array{prefixed_array = Nodes}} = decode_prefixed_array(Data4, debug_path_node),
    {Data6, #prefixed_array{prefixed_array = TargetNodes}} = decode_prefixed_array(Data5, debug_path_node),
    {Data7, #prefixed_array{prefixed_array = OpenSet}} = decode_prefixed_array(Data6, debug_path_node),
    {Data8, #prefixed_array{prefixed_array = ClosedSet}} = decode_prefixed_array(Data7, debug_path_node),
    {Data9, #float{float = MaxNodeDistance}} = decode_float(Data8),
    {Data9, #entity_path{type = entity_path, reached = Reached, next_block_index = NextBlockIndex,
        block_position = BlockPosition, nodes = Nodes, target_nodes = TargetNodes,
        open_set = OpenSet, closed_set = ClosedSet, max_node_distance = MaxNodeDistance}}.

decode_debug_path_node(Data) ->
    {Data2, #int{int = X}} = decode_int(Data),
    {Data3, #int{int = Y}} = decode_int(Data2),
    {Data4, #int{int = Z}} = decode_int(Data3),
    {Data5, #float{float = WalkCost}} = decode_float(Data4),
    {Data6, #float{float = Penalty}} = decode_float(Data5),
    {Data7, #bool{bool = Open}} = decode_bool(Data6),
    {Data8, #varint{varint = Type}} = decode_varint(Data7),
    {Data9, #int{int = HeapIndex}} = decode_int(Data8),
    {Data9, #debug_path_node{x = X, y = Y, z = Z, walk_cost = WalkCost, penalty = Penalty, open = Open, type = Type, heap_index = HeapIndex}}.

entity_block_intersection(Data) ->
    {Data2, EnumRec} = decode_enum(Data),
    ID = extract_value(EnumRec),
    {Data2, #entity_block_intersection{type = entity_block_intersection, id = ID}}.

bee_hive(Data) ->
    {Data2, EnumRec} = decode_enum(Data),
    HiveType = extract_value(EnumRec),
    {Data3, #varint{varint = OccupantCount}} = decode_varint(Data2),
    {Data4, #varint{varint = HoneyLevel}} = decode_varint(Data3),
    {Data5, #bool{bool = Sedated}} = decode_bool(Data4),
    {Data5, #bee_hive{type = bee_hive, hive_type = HiveType, occupant_count = OccupantCount,
        honey_level = HoneyLevel, sedated = Sedated}}.

poi(Data) ->
    {Data2, Position} = decode_position(Data),
    {Data3, EnumRec} = decode_enum(Data2),
    POIType = extract_value(EnumRec),
    {Data4, #varint{varint = FreeTicketCount}} = decode_varint(Data3),
    {Data4, #poi{type = poi, position = Position, poi_type = POIType, free_ticket_count = FreeTicketCount}}.

redstone_wire_orientation(Data) ->
    {Data2, #varint{varint = ID}} = decode_varint(Data),
    {Data2, #redstone_wire_orientation{type = redstone_wire_orientation, id = ID}}.

raid(Data) ->
    {Data2, #prefixed_array{prefixed_array = Positions}} = decode_prefixed_array(Data, position),
    {Data2, #raid{type = raid, positions = Positions}}.

structure(Data) ->
    {Data2, #prefixed_array{prefixed_array = Structures}} = decode_prefixed_array(Data, debug_structure_info),
    {Data2, #structure{type = structure, structures = Structures}}.

decode_debug_structure_info(Data) ->
    {Data2, #int{int = MinX}} = decode_int(Data),
    {Data3, #int{int = MinY}} = decode_int(Data2),
    {Data4, #int{int = MinZ}} = decode_int(Data3),
    {Data5, #int{int = MaxX}} = decode_int(Data4),
    {Data6, #int{int = MaxY}} = decode_int(Data5),
    {Data7, #int{int = MaxZ}} = decode_int(Data6),
    {Data8, #prefixed_array{prefixed_array = Pieces}} = decode_prefixed_array(Data7, debug_structure_piece),
    {Data8, #debug_structure_info{min_x = MinX, min_y = MinY, min_z = MinZ, max_x = MaxX, max_y = MaxY, max_z = MaxZ, pieces = Pieces}}.

decode_debug_structure_piece(Data) ->
    {Data2, #int{int = MinX}} = decode_int(Data),
    {Data3, #int{int = MinY}} = decode_int(Data2),
    {Data4, #int{int = MinZ}} = decode_int(Data3),
    {Data5, #int{int = MaxX}} = decode_int(Data4),
    {Data6, #int{int = MaxY}} = decode_int(Data5),
    {Data7, #int{int = MaxZ}} = decode_int(Data6),
    {Data8, #bool{bool = IsStart}} = decode_bool(Data7),
    {Data8, #debug_structure_piece{min_x = MinX, min_y = MinY, min_z = MinZ, max_x = MaxX, max_y = MaxY, max_z = MaxZ, is_start = IsStart}}.

game_event_listener(Data) ->
    {Data2, #varint{varint = ListenerRadius}} = decode_varint(Data),
    {Data2, #game_event_listener{type = game_event_listener, listener_radius = ListenerRadius}}.

neighbor_update(Data) ->
    {Data2, Position} = decode_position(Data),
    {Data2, #neighbor_update{type = neighbor_update, position = Position}}.

game_event(Data) ->
    {Data2, EnumRec} = decode_enum(Data),
    Event = extract_value(EnumRec),
    {Data3, #double{double = X}} = decode_double(Data2),
    {Data4, #double{double = Y}} = decode_double(Data3),
    {Data5, #double{double = Z}} = decode_double(Data4),
    {Data5, #game_event{type = game_event, event = Event, x = X, y = Y, z = Z}}.

decode_lp_vec3(<<0:8, Rest/binary>>) ->
    {Rest, #lp_vec3{x = 0.0, y = 0.0, z = 0.0}};
decode_lp_vec3(<<Byte1:8, Byte2:8, Bytes3To6:32/unsigned-integer-big, Rest0/binary>>) ->
    Packed = (Bytes3To6 bsl 16) bor (Byte2 bsl 8) bor Byte1,
    ScaleFactor0 = Byte1 band 3,
    Res = case (Byte1 band 4) =/= 0 of
        true ->
            case decode_varint(Rest0) of
                {R, #varint{varint = VarIntVal}} ->
                    {ok, R, ScaleFactor0 bor (VarIntVal bsl 2)};
                Error ->
                    Error
            end;
        false ->
            {ok, Rest0, ScaleFactor0}
    end,
    case Res of
        {error, _} = ErrorRes ->
            ErrorRes;
        {ok, Rest1, ScaleFactor} ->
            ScaleFactorD = float(ScaleFactor),
            X = unpack_lp_vec3(Packed bsr 3) * ScaleFactorD,
            Y = unpack_lp_vec3(Packed bsr 18) * ScaleFactorD,
            Z = unpack_lp_vec3(Packed bsr 33) * ScaleFactorD,
            {Rest1, #lp_vec3{x = X, y = Y, z = Z}}
    end;
decode_lp_vec3(_) ->
    {error, "insufficient data"}.

unpack_lp_vec3(Value) ->
    V = Value band 32767,
    VClamped = min(float(V), 32766.0),
    VClamped * 2.0 / 32766.0 - 1.0.

decode_seen_advancements(Data) ->
    {Data2, Action} = decode_type(Data, {enum, varint}),
    IsTrue = extract_value(Action) =:= 0,
    {Data3, TabId} = decode_type(Data2, {optional, identifier, IsTrue}),
    {Data3, #seen_advancements{type = seen_advancements, action = Action, tab_id = TabId}}.
        
decode_boss_bar(Data) ->
    {Data2, UUIDRec} = decode_type(Data, uuid),
    UUID = extract_value(UUIDRec),
    {Data3, ActionRec} = decode_type(Data2, varint),
    Action = extract_value(ActionRec),
    case Action of
        0 -> 
            {Data4, Title} = decode_type(Data3, text_component),
            {Data5, HealthRec} = decode_type(Data4, float),
            Health = extract_value(HealthRec),
            {Data6, EnumRec} = decode_enum(Data5),
            Color = extract_value(EnumRec),
            {Data7, EnumRec2} = decode_enum(Data6),
            Division = extract_value(EnumRec2),
            {Data8, FlagsRec} = decode_type(Data7, ubyte),
            Flags = extract_value(FlagsRec),
            {Data8, #boss_bar{type = boss_bar, uuid = UUID, action = 0, title = Title, health = Health, color = Color, division = Division, flags = Flags}};
        1 ->
            {Data3, #boss_bar{type = boss_bar, uuid = UUID, action = 1}};
        2 ->
            {Data4, HealthRec} = decode_type(Data3, float),
            Health = extract_value(HealthRec),
            {Data4, #boss_bar{type = boss_bar, uuid = UUID, action = 2, health = Health}};
        3 ->
            {Data4, Title} = decode_type(Data3, text_component),
            {Data4, #boss_bar{type = boss_bar, uuid = UUID, action = 3, title = Title}};
        4 ->
            {Data4, EnumRec} = decode_enum(Data3),
            Color = extract_value(EnumRec),
            {Data5, EnumRec2} = decode_enum(Data4),
            Division = extract_value(EnumRec2),
            {Data5, #boss_bar{type = boss_bar, uuid = UUID, action = 4, color = Color, division = Division}};
        5 ->
            {Data4, FlagsRec} = decode_type(Data3, ubyte),
            Flags = extract_value(FlagsRec),
            {Data4, #boss_bar{type = boss_bar, uuid = UUID, action = 5, flags = Flags}};
        _ ->
            error({unknown_boss_bar_action, Action})
    end.


decode_node(Data) ->
    {Data2, #byte{byte = Flags}} = decode_byte(Data),
    UnsignedFlags = Flags band 16#FF,
    NodeType = UnsignedFlags band 16#03,
    {Data3, #varint{varint = ChildrenCount}} = decode_varint(Data2),
    {Data4, #array{array = Children}} = decode_array(Data3, ChildrenCount, varint),
    HasRedirect = (UnsignedFlags band 16#08) =/= 0,
    {Data5, RedirectNode} = decode_optional(Data4, varint, HasRedirect),
    HasName = (NodeType =:= 1) orelse (NodeType =:= 2),
    {Data6, Name} = decode_optional(Data5, string, HasName),
    IsArgument = (NodeType =:= 2),
    {Data7, ParserID} = decode_optional(Data6, varint, IsArgument),
    {Data8, Properties} = case IsArgument of
        true ->
            PID = extract_value(ParserID),
            {RestData, PropVal} = decode_parser_properties(Data7, PID),
            {RestData, #optional{some = some, optional = PropVal}};
        false ->
            {Data7, #optional{some = none, optional = none}}
    end,

    HasSuggestions = (UnsignedFlags band 16#10) =/= 0,
    {Data9, SuggestionsType} = decode_optional(Data8, identifier, HasSuggestions),
    {Data9, #node{
        flags = Flags,
        children_count = ChildrenCount,
        children = Children,
        redirect_node = RedirectNode,
        name = Name,
        parser_id = ParserID,
        properties = Properties,
        suggestions_type = SuggestionsType
    }}.

decode_parser_properties(Data, 1) -> % brigadier:float
    {Data2, #byte{byte = Flags}} = decode_byte(Data),
    HasMin = (Flags band 16#01) =/= 0,
    {Data3, Min} = decode_optional(Data2, float, HasMin),
    HasMax = (Flags band 16#02) =/= 0,
    {Data4, Max} = decode_optional(Data3, float, HasMax),
    {Data4, {Flags, Min, Max}};
decode_parser_properties(Data, 2) -> % brigadier:double
    {Data2, #byte{byte = Flags}} = decode_byte(Data),
    HasMin = (Flags band 16#01) =/= 0,
    {Data3, Min} = decode_optional(Data2, double, HasMin),
    HasMax = (Flags band 16#02) =/= 0,
    {Data4, Max} = decode_optional(Data3, double, HasMax),
    {Data4, {Flags, Min, Max}};
decode_parser_properties(Data, 3) -> % brigadier:integer
    {Data2, #byte{byte = Flags}} = decode_byte(Data),
    HasMin = (Flags band 16#01) =/= 0,
    {Data3, Min} = decode_optional(Data2, int, HasMin),
    HasMax = (Flags band 16#02) =/= 0,
    {Data4, Max} = decode_optional(Data3, int, HasMax),
    {Data4, {Flags, Min, Max}};
decode_parser_properties(Data, 4) -> % brigadier:long
    {Data2, #byte{byte = Flags}} = decode_byte(Data),
    HasMin = (Flags band 16#01) =/= 0,
    {Data3, Min} = decode_optional(Data2, long, HasMin),
    HasMax = (Flags band 16#02) =/= 0,
    {Data4, Max} = decode_optional(Data3, long, HasMax),
    {Data4, {Flags, Min, Max}};
decode_parser_properties(Data, 5) -> % brigadier:string
    {Data2, #varint{varint = Behavior}} = decode_varint(Data),
    {Data2, Behavior};
decode_parser_properties(Data, 6) -> % minecraft:entity
    {Data2, #byte{byte = Flags}} = decode_byte(Data),
    {Data2, Flags};
decode_parser_properties(Data, 23) -> % minecraft:time
    {Data2, #int{int = Min}} = decode_int(Data),
    {Data2, Min};
decode_parser_properties(Data, PID) when PID >= 24, PID =< 27 -> % resource/tag
    {Data2, #identifier{identifier = Registry}} = decode_identifier(Data),
    {Data2, Registry};
decode_parser_properties(Data, 34) -> % minecraft:score_holder
    {Data2, #byte{byte = Flags}} = decode_byte(Data),
    {Data2, Flags};
decode_parser_properties(Data, _PID) ->
    {Data, none}.

decode_delete_chat(Data) ->
    {Data2, MsgIdRec} = decode_type(Data, varint),
    MsgIdVal = extract_value(MsgIdRec),
    IsTrue = (MsgIdVal =:= 0),
    {Data3, Signature} = decode_optional(Data2, {byte_array, 256}, IsTrue),
    {Data3, #delete_chat{message_id = MsgIdRec, signature = Signature}}.

decode_chat_type(Data) ->
    {Data2, TranslationKey} = decode_type(Data, string),
    {Data3, Parameters} = decode_type(Data2, {prefixed_array, {enum, [sender, target, content]}}),
    {Data4, Style} = decode_type(Data3, nbt),
    {Data4, #chat_type{translation_key = TranslationKey, parameters = Parameters, style = Style}}.

decode_player_info_update(Data) ->
    {Data2, ActionsRec} = decode_type(Data, ubyte),
    Actions = extract_value(ActionsRec),
    {Data3, CountRec} = decode_type(Data2, varint),
    Count = extract_value(CountRec),
    {Data4, Players} = decode_player_info_entries(Data3, Count, Actions, []),
    {Data4, #player_info_update{actions = ActionsRec, players = Players}}.

decode_player_info_entries(Data, 0, _Actions, Acc) ->
    {Data, lists:reverse(Acc)};
decode_player_info_entries(Data, Count, Actions, Acc) ->
    {Data2, UUIDRec} = decode_type(Data, uuid),
    {Data3, PlayerActions} = decode_player_actions(Data2, Actions),
    Entry = #player_info_entry{uuid = UUIDRec, actions = PlayerActions},
    decode_player_info_entries(Data3, Count - 1, Actions, [Entry | Acc]).

decode_player_actions(Data, Actions) ->
    ActionBits = [
        {16#01, fun decode_action_add_player/1},
        {16#02, fun decode_action_initialize_chat/1},
        {16#04, fun decode_action_update_game_mode/1},
        {16#08, fun decode_action_update_listed/1},
        {16#10, fun decode_action_update_latency/1},
        {16#20, fun decode_action_update_display_name/1},
        {16#40, fun decode_action_update_list_order/1}
    ],
    decode_player_actions_loop(Data, Actions, ActionBits, []).

decode_player_actions_loop(Data, _Actions, [], Acc) ->
    {Data, lists:reverse(Acc)};
decode_player_actions_loop(Data, Actions, [{Bit, DecodeFun} | Rest], Acc) ->
    case (Actions band Bit) =/= 0 of
        true ->
            {Data2, ActionVal} = DecodeFun(Data),
            decode_player_actions_loop(Data2, Actions, Rest, [ActionVal | Acc]);
        false ->
            decode_player_actions_loop(Data, Actions, Rest, Acc)
    end.

decode_action_add_player(Data) ->
    {Data2, NameRec} = decode_type(Data, string),
    {Data3, PropsCountRec} = decode_type(Data2, varint),
    Count = extract_value(PropsCountRec),
    {Data4, Properties} = decode_properties(Data3, Count),
    {Data4, {add_player, NameRec, Properties}}.

decode_action_initialize_chat(Data) ->
    {Data2, ChatSession} = decode_prefixed_optional(Data, [uuid, long, byte_array, byte_array]),
    {Data2, {initialize_chat, ChatSession}}.

decode_action_update_game_mode(Data) ->
    {Data2, GameModeRec} = decode_type(Data, varint),
    {Data2, {update_game_mode, GameModeRec}}.

decode_action_update_listed(Data) ->
    {Data2, ListedRec} = decode_type(Data, bool),
    {Data2, {update_listed, ListedRec}}.

decode_action_update_latency(Data) ->
    {Data2, LatencyRec} = decode_type(Data, varint),
    {Data2, {update_latency, LatencyRec}}.

decode_action_update_display_name(Data) ->
    {Data2, DisplayName} = decode_prefixed_optional(Data, json_text_component),
    {Data2, {update_display_name, DisplayName}}.

decode_action_update_list_order(Data) ->
    {Data2, ListOrderRec} = decode_type(Data, varint),
    {Data2, {update_list_order, ListOrderRec}}.

decode_set_equipment(Data) ->
    {Data2, EntityID} = decode_type(Data, varint),
    {Data3, Equipment} = decode_equipment_list(Data2, []),
    {Data3, #set_equipment{entity_id = EntityID, equipment = Equipment}}.

decode_equipment_list(Data, Acc) ->
    {Data2, #byte{byte = RawByte}} = decode_byte(Data),
    UByte = RawByte band 16#FF,
    HasNext = (UByte band 16#80) =/= 0,
    SlotEnumVal = UByte band 16#7F,
    {Data3, ItemSlot} = decode_type(Data2, slot),
    Entry = {#enum{enum = SlotEnumVal}, ItemSlot},
    NewAcc = [Entry | Acc],
    case HasNext of
        true ->
            decode_equipment_list(Data3, NewAcc);
        false ->
            {Data3, lists:reverse(NewAcc)}
    end.

decode_set_objective(Data) ->
    {Data2, NameRec} = decode_type(Data, string),
    ObjectiveName = extract_value(NameRec),
    {Data3, ModeRec} = decode_type(Data2, byte),
    Mode = extract_value(ModeRec),
    case Mode of
        1 ->
            {Data3, #set_objective{
                objective_name = ObjectiveName,
                mode = Mode,
                objective_value = undefined,
                type = undefined,
                number_format = undefined
            }};
        _ when Mode =:= 0; Mode =:= 2 ->
            {Data4, ObjectiveValue} = decode_type(Data3, text_component),
            {Data5, TypeRec} = decode_type(Data4, {enum, varint}),
            Type = extract_value(TypeRec),
            {Data6, HasNumFormatRec} = decode_type(Data5, bool),
            HasNumFormat = extract_value(HasNumFormatRec),
            {Data7, NumberFormat} = case HasNumFormat of
                true ->
                    {Data6_1, FormatIDRec} = decode_type(Data6, {enum, varint}),
                    FormatID = extract_value(FormatIDRec),
                    case FormatID of
                        0 ->
                            {Data6_1, blank};
                        1 ->
                            {Data6_2, Styling} = decode_type(Data6_1, nbt),
                            {Data6_2, {styled, Styling}};
                        2 ->
                            {Data6_2, Content} = decode_type(Data6_1, text_component),
                            {Data6_2, {fixed, Content}};
                        _ ->
                            error({unknown_number_format_id, FormatID})
                    end;
                false ->
                    {Data6, undefined}
            end,
            {Data7, #set_objective{
                objective_name = ObjectiveName,
                mode = Mode,
                objective_value = ObjectiveValue,
                type = Type,
                number_format = NumberFormat
            }};
        _ ->
            error({unknown_set_objective_mode, Mode})
    end.

decode_set_player_team(Data) ->
    {Data2, NameRec} = decode_type(Data, string),
    TeamName = extract_value(NameRec),
    {Data3, MethodRec} = decode_type(Data2, byte),
    Method = extract_value(MethodRec),
    case Method of
        0 ->
            {Data4, TeamDisplayName} = decode_type(Data3, text_component),
            {Data5, TeamPrefix} = decode_type(Data4, text_component),
            {Data6, TeamSuffix} = decode_type(Data5, text_component),
            {Data7, NameTagVisRec} = decode_type(Data6, {enum, varint}),
            NameTagVis = extract_value(NameTagVisRec),
            {Data8, CollisionRuleRec} = decode_type(Data7, {enum, varint}),
            CollisionRule = extract_value(CollisionRuleRec),
            {Data9, TeamColorRec} = decode_type(Data8, {enum, varint}),
            TeamColor = extract_value(TeamColorRec),
            {Data10, FriendlyFlagsRec} = decode_type(Data9, byte),
            FriendlyFlags = extract_value(FriendlyFlagsRec),
            {Data11, Entities} = decode_type(Data10, {prefixed_array, string}),
            {Data11, #set_player_team{
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
            }};
        1 ->
            {Data3, #set_player_team{
                team_name = TeamName,
                method = Method,
                team_display_name = undefined,
                team_prefix = undefined,
                team_suffix = undefined,
                name_tag_visibility = undefined,
                collision_rule = undefined,
                team_color = undefined,
                friendly_flags = undefined,
                entities = undefined
            }};
        2 ->
            {Data4, TeamDisplayName} = decode_type(Data3, text_component),
            {Data5, TeamPrefix} = decode_type(Data4, text_component),
            {Data6, TeamSuffix} = decode_type(Data5, text_component),
            {Data7, NameTagVisRec} = decode_type(Data6, {enum, varint}),
            NameTagVis = extract_value(NameTagVisRec),
            {Data8, CollisionRuleRec} = decode_type(Data7, {enum, varint}),
            CollisionRule = extract_value(CollisionRuleRec),
            {Data9, TeamColorRec} = decode_type(Data8, {enum, varint}),
            TeamColor = extract_value(TeamColorRec),
            {Data10, FriendlyFlagsRec} = decode_type(Data9, byte),
            FriendlyFlags = extract_value(FriendlyFlagsRec),
            {Data10, #set_player_team{
                team_name = TeamName,
                method = Method,
                team_display_name = TeamDisplayName,
                team_prefix = TeamPrefix,
                team_suffix = TeamSuffix,
                name_tag_visibility = NameTagVis,
                collision_rule = CollisionRule,
                team_color = TeamColor,
                friendly_flags = FriendlyFlags,
                entities = undefined
            }};
        3 ->
            {Data4, Entities} = decode_type(Data3, {prefixed_array, string}),
            {Data4, #set_player_team{
                team_name = TeamName,
                method = Method,
                team_display_name = undefined,
                team_prefix = undefined,
                team_suffix = undefined,
                name_tag_visibility = undefined,
                collision_rule = undefined,
                team_color = undefined,
                friendly_flags = undefined,
                entities = Entities
            }};
        4 ->
            {Data4, Entities} = decode_type(Data3, {prefixed_array, string}),
            {Data4, #set_player_team{
                team_name = TeamName,
                method = Method,
                team_display_name = undefined,
                team_prefix = undefined,
                team_suffix = undefined,
                name_tag_visibility = undefined,
                collision_rule = undefined,
                team_color = undefined,
                friendly_flags = undefined,
                entities = Entities
            }};
        _ ->
            error({unknown_set_player_team_method, Method})
    end.

decode_waypoint_data(Data) ->
    {Data2, WaypointTypeRec} = decode_type(Data, {enum, varint}),
    WaypointType = extract_value(WaypointTypeRec),
    case WaypointType of
        0 ->
            {Data2, #waypoint_data{waypoint_type = 0}};
        1 ->
            {Data3, XRec} = decode_type(Data2, varint),
            {Data4, YRec} = decode_type(Data3, varint),
            {Data5, ZRec} = decode_type(Data4, varint),
            {Data5, #waypoint_data{
                waypoint_type = 1,
                x = extract_value(XRec),
                y = extract_value(YRec),
                z = extract_value(ZRec)
            }};
        2 ->
            {Data3, XRec} = decode_type(Data2, varint),
            {Data4, ZRec} = decode_type(Data3, varint),
            {Data4, #waypoint_data{
                waypoint_type = 2,
                x = extract_value(XRec),
                z = extract_value(ZRec)
            }};
        3 ->
            {Data3, AngleRec} = decode_type(Data2, float),
            {Data3, #waypoint_data{
                waypoint_type = 3,
                angle = extract_value(AngleRec)
            }};
        _ ->
            error({unknown_waypoint_type, WaypointType})
    end.

decode_stop_sound(Data) ->
    {Data2, FlagsRec} = decode_type(Data, byte),
    Flags = extract_value(FlagsRec),
    {Data3, Source} = case (Flags band 1) =/= 0 of
        true ->
            {D2, SRec} = decode_type(Data2, {enum, varint}),
            {D2, extract_value(SRec)};
        false ->
            {Data2, undefined}
    end,
    {Data4, Sound} = case (Flags band 2) =/= 0 of
        true ->
            {D3, SndRec} = decode_type(Data3, identifier),
            {D3, extract_value(SndRec)};
        false ->
            {Data3, undefined}
    end,
    {Data4, #stop_sound{
        flags = Flags,
        source = Source,
        sound = Sound
    }}.

decode_set_score(Data) ->
    {Data2, EntityNameRec} = decode_type(Data, string),
    EntityName = extract_value(EntityNameRec),
    {Data3, ObjectiveNameRec} = decode_type(Data2, string),
    ObjectiveName = extract_value(ObjectiveNameRec),
    {Data4, ValueRec} = decode_type(Data3, varint),
    Value = extract_value(ValueRec),
    {Data5, HasDisplayNameRec} = decode_type(Data4, bool),
    HasDisplayName = extract_value(HasDisplayNameRec),
    {Data6, DisplayName} = case HasDisplayName of
        true ->
            decode_type(Data5, text_component);
        false ->
            {Data5, undefined}
    end,
    {Data7, HasNumberFormatRec} = decode_type(Data6, bool),
    HasNumberFormat = extract_value(HasNumberFormatRec),
    {Data8, NumberFormat} = case HasNumberFormat of
        true ->
            {Data7_1, FormatIDRec} = decode_type(Data7, {enum, varint}),
            FormatID = extract_value(FormatIDRec),
            case FormatID of
                0 ->
                    {Data7_1, blank};
                1 ->
                    {Data7_2, Styling} = decode_type(Data7_1, nbt),
                    {Data7_2, {styled, Styling}};
                2 ->
                    {Data7_2, Content} = decode_type(Data7_1, text_component),
                    {Data7_2, {fixed, Content}};
                _ ->
                    error({unknown_number_format_id, FormatID})
            end;
        false ->
            {Data7, undefined}
    end,
    {Data8, #set_score{
        entity_name = EntityName,
        objective_name = ObjectiveName,
        value = Value,
        display_name = DisplayName,
        number_format = NumberFormat
    }}.

decode_update_advancements(Data) ->
    {Data2, ResetRec}      = decode_type(Data,  bool),
    Reset                   = extract_value(ResetRec),
    {Data3, MappingRec}    = decode_type(Data2, {prefixed_array, [identifier, advancement]}),
    AdvancementMapping      = MappingRec#prefixed_array.prefixed_array,
    {Data4, IdentRec}      = decode_type(Data3, {prefixed_array, identifier}),
    Identifiers             = IdentRec#prefixed_array.prefixed_array,
    {Data5, ProgressRec}   = decode_type(Data4, {prefixed_array, [identifier, advancement_progress]}),
    ProgressMapping         = ProgressRec#prefixed_array.prefixed_array,
    {Data5, #update_advancements{
        reset               = Reset,
        advancement_mapping = AdvancementMapping,
        identifiers         = Identifiers,
        progress_mapping    = ProgressMapping
    }}.

decode_advancement(Data) ->
    {Data2, ParentId}      = decode_type(Data,  {prefixed_optional, identifier}),
    {Data3, HasDisplayRec} = decode_type(Data2, bool),
    HasDisplay              = extract_value(HasDisplayRec),
    {Data4, DisplayData}   = case HasDisplay of
        true  -> decode_advancement_display(Data3);
        false -> {Data3, none}
    end,
    {Data5, ReqRec}        = decode_type(Data4, {prefixed_array, {prefixed_array, string}}),
    Requirements            = ReqRec#prefixed_array.prefixed_array,
    {Data6, TelRec}        = decode_type(Data5, bool),
    SendsTelemetry          = extract_value(TelRec),
    {Data6, #advancement{
        parent_id       = ParentId,
        display_data    = DisplayData,
        requirements    = Requirements,
        sends_telemetry = SendsTelemetry
    }}.

decode_advancement_display(Data) ->
    {Data2, TitleRec}      = decode_type(Data,  text_component),
    {Data3, DescRec}       = decode_type(Data2, text_component),
    {Data4, IconRec}       = decode_type(Data3, slot),
    {Data5, FrameRec}      = decode_type(Data4, varint),
    FrameType               = extract_value(FrameRec),
    {Data6, FlagsRec}      = decode_type(Data5, int),
    Flags                   = extract_value(FlagsRec),
    {Data7, BgTexture}     = case (Flags band 16#01) =/= 0 of
        true  ->
            {D6, BgRec} = decode_type(Data6, identifier),
            {D6, extract_value(BgRec)};
        false ->
            {Data6, undefined}
    end,
    {Data8, XRec}          = decode_type(Data7, float),
    X                       = extract_value(XRec),
    {Data9, YRec}          = decode_type(Data8, float),
    Y                       = extract_value(YRec),
    {Data9, #advancement_display{
        title              = TitleRec,
        description        = DescRec,
        icon               = IconRec,
        frame_type         = FrameType,
        flags              = Flags,
        background_texture = BgTexture,
        x                  = X,
        y                  = Y
    }}.

decode_advancement_progress(Data) ->
    {Data2, CriteriaRec} = decode_type(Data, {prefixed_array, [identifier, {prefixed_optional, long}]}),
    Criteria              = CriteriaRec#prefixed_array.prefixed_array,
    {Data2, #advancement_progress{
        criteria = Criteria
    }}.
