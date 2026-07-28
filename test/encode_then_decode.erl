-module(encode_then_decode).
-include_lib("eunit/include/eunit.hrl").
-include("src/data_types/records.hrl").
-include("src/data_types/components/component_records.hrl").


bool_test() ->
    {<<>>, #bool{bool = true}} = decode:decode_type(encode:encode_type(true, bool), bool),
    {<<>>, #bool{bool = false}} = decode:decode_type(encode:encode_type(false, bool), bool).

byte_test() ->
    {<<>>, #byte{byte = 42}} = decode:decode_type(encode:encode_type(42, byte), byte),
    {<<>>, #byte{byte = -42}} = decode:decode_type(encode:encode_type(-42, byte), byte).

ubyte_test() ->
    {<<>>, #ubyte{ubyte = 200}} = decode:decode_type(encode:encode_type(200, ubyte), ubyte).

short_test() ->
    {<<>>, #short{short = 12345}} = decode:decode_type(encode:encode_type(12345, short), short),
    {<<>>, #short{short = -12345}} = decode:decode_type(encode:encode_type(-12345, short), short).

ushort_test() ->
    {<<>>, #ushort{ushort = 50000}} = decode:decode_type(encode:encode_type(50000, ushort), ushort).

int_test() ->
    {<<>>, #int{int = 1000000}} = decode:decode_type(encode:encode_type(1000000, int), int),
    {<<>>, #int{int = -1000000}} = decode:decode_type(encode:encode_type(-1000000, int), int).

long_test() ->
    Val = 9223372036854775807,
    {<<>>, #long{long = Val}} = decode:decode_type(encode:encode_type(Val, long), long).

float_test() ->
    Val = 3.14159,
    Encoded = encode:encode_type(Val, float),
    {<<>>, #float{float = Decoded}} = decode:decode_type(Encoded, float),
    ?assert(abs(Val - Decoded) < 0.0001).

double_test() ->
    Val = 3.141592653589793,
    Encoded = encode:encode_type(Val, double),
    {<<>>, #double{double = Decoded}} = decode:decode_type(Encoded, double),
    ?assertEqual(Val, Decoded).

string_test() ->
    Str = "Hello Minecraft!",
    Encoded = encode:encode_type(Str, string),
    {<<>>, #string{string = Decoded}} = decode:decode_type(Encoded, string),
    ?assertEqual(Str, Decoded),
    EncodedBin = encode:encode_type(<<"Hello Minecraft!">>, string),
    {<<>>, #string{string = Decoded2}} = decode:decode_type(EncodedBin, string),
    ?assertEqual(Str, Decoded2).

json_text_component_test() ->
    Map = #{<<"text">> => <<"Hello world">>, <<"color">> => <<"blue">>},
    EncodedMap = encode:encode_type(Map, json_text_component),
    {<<>>, #json_text_component{json_component_map = DecodedMap}} = decode:decode_type(EncodedMap, json_text_component),
    ?assertEqual(Map, DecodedMap),

    RawJson = <<"{\"text\":\"Hello\",\"bold\":true}">>,
    EncodedRaw = encode:encode_type(RawJson, json_text_component),
    {<<>>, #json_text_component{json_component_map = DecodedRaw}} = decode:decode_type(EncodedRaw, json_text_component),
    ?assertEqual(#{<<"text">> => <<"Hello">>, <<"bold">> => true}, DecodedRaw),

    CompList = [#{<<"text">> => <<"Part 1">>}, #{<<"text">> => <<"Part 2">>}],
    EncodedList = encode:encode_type(CompList, json_text_component),
    {<<>>, #json_text_component{json_component_map = DecodedList}} = decode:decode_type(EncodedList, json_text_component),
    ?assertEqual(CompList, DecodedList).

varint_test() ->
    {<<>>, #varint{varint = 0}} = decode:decode_type(encode:encode_type(0, varint), varint),
    {<<>>, #varint{varint = 128}} = decode:decode_type(encode:encode_type(128, varint), varint),
    {<<>>, #varint{varint = 300}} = decode:decode_type(encode:encode_type(300, varint), varint),
    {<<>>, #varint{varint = 2147483647}} = decode:decode_type(encode:encode_type(2147483647, varint), varint),
    {<<>>, #varint{varint = -1}} = decode:decode_type(encode:encode_type(-1, varint), varint),
    {<<>>, #varint{varint = -2147483648}} = decode:decode_type(encode:encode_type(-2147483648, varint), varint).


varlong_test() ->
    {<<>>, #varlong{varlong = 0}} = decode:decode_type(encode:encode_type(0, varlong), varlong),
    {<<>>, #varlong{varlong = 128}} = decode:decode_type(encode:encode_type(128, varlong), varlong),
    {<<>>, #varlong{varlong = 300}} = decode:decode_type(encode:encode_type(300, varlong), varlong),
    {<<>>, #varlong{varlong = 2147483647}} = decode:decode_type(encode:encode_type(2147483647, varlong), varlong),
    {<<>>, #varlong{varlong = 9223372036854775807}} = decode:decode_type(encode:encode_type(9223372036854775807, varlong), varlong),
    {<<>>, #varlong{varlong = -1}} = decode:decode_type(encode:encode_type(-1, varlong), varlong),
    {<<>>, #varlong{varlong = -9223372036854775808}} = decode:decode_type(encode:encode_type(-9223372036854775808, varlong), varlong).

identifier_test() ->
    Str = "minecraft:diamond",
    Encoded = encode:encode_type(Str, identifier),
    {<<>>, #identifier{identifier = Decoded}} = decode:decode_type(Encoded, identifier),
    ?assertEqual(Str, Decoded).

position_test() ->
    Pos = {100, -200, 50},
    Encoded = encode:encode_type(Pos, position),
    {<<>>, #position{x = X, z = Z, y = Y}} = decode:decode_type(Encoded, position),
    ?assertEqual(Pos, {X, Z, Y}).

angle_test() ->
    Angle = 45,
    Encoded = encode:encode_type(Angle, angle),
    {<<>>, #angle{angle = Decoded}} = decode:decode_type(Encoded, angle),
    ?assertEqual(Angle, Decoded).

uuid_test() ->
    UUID = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    Encoded = encode:encode_type(UUID, uuid),
    {<<>>, #uuid{uuid = Decoded}} = decode:decode_type(Encoded, uuid),
    ?assertEqual(UUID, Decoded).

bitset_test() ->
    Val1 = -1,
    Encoded1 = encode:encode_type(Val1, bitset),
    {<<>>, #bitset{bitset = Decoded1}} = decode:decode_type(Encoded1, bitset),
    ?assertEqual(Val1, Decoded1),
    Val2 = 256,
    Encoded2 = encode:encode_type(Val2, bitset),
    {<<>>, #bitset{bitset = Decoded2}} = decode:decode_type(Encoded2, bitset),
    ?assertEqual(Val2, Decoded2).

fixed_bitset_test() ->
    Val1 = -1,
    Encoded1 = encode:encode_type(Val1, fixed_bitset),
    {<<>>, #fixed_bitset{fixed_bitset = Decoded1}} = decode:decode_type(Encoded1, fixed_bitset),
    ?assertEqual(Val1, Decoded1),
    Val2 = 256,
    Encoded2 = encode:encode_type(Val2, fixed_bitset),
    {<<>>, #fixed_bitset{fixed_bitset = Decoded2}} = decode:decode_type(Encoded2, fixed_bitset),
    ?assertEqual(Val2, Decoded2).

optional_test() ->
    EncPresent = encode:encode_type(42, {optional, int, true}),
    ?assertEqual({<<>>, #optional{some = some, optional = #int{int = 42}}}, decode:decode_type(EncPresent, {optional, int, true})),

    EncAbsent = encode:encode_type(42, {optional, int, false}),
    ?assertEqual({<<>>, #optional{some = none, optional = none}}, decode:decode_type(EncAbsent, {optional, int, false})),

    EncTaggedSome = encode:encode_type({some, "hello"}, {optional, string, true}),
    ?assertEqual({<<>>, #optional{some = some, optional = #string{string = "hello"}}}, decode:decode_type(EncTaggedSome, {optional, string, true})),

    EncTaggedNone = encode:encode_type(none, {optional, string, false}),
    ?assertEqual({<<>>, #optional{some = none, optional = none}}, decode:decode_type(EncTaggedNone, {optional, string, false})).

prefixed_optional_test() ->
    EncPresent = encode:encode_type({some, 42}, {optional, int}),
    ?assertEqual({<<>>, #prefixed_optional{some = some, prefixed_optional = #int{int = 42}}}, decode:decode_type(EncPresent, {optional, int})),

    EncAbsent = encode:encode_type(none, {optional, int}),
    ?assertEqual({<<>>, #prefixed_optional{some = none, prefixed_optional = none}}, decode:decode_type(EncAbsent, {optional, int})),

    EncVal = encode:encode_type("hello", {optional, string}),
    ?assertEqual({<<>>, #prefixed_optional{some = some, prefixed_optional = #string{string = "hello"}}}, decode:decode_type(EncVal, {optional, string})).

id_or_x_test() ->
    IdRef = {id, 5},
    Enc1 = encode:encode_type(IdRef, {id_or_x, string}),
    ?assertEqual({<<>>, #id_or_x{id_or_x = 5}}, decode:decode_type(Enc1, {id_or_x, string})),

    InlineVal = {val, "hello"},
    Enc2 = encode:encode_type(InlineVal, {id_or_x, string}),
    ?assertEqual({<<>>, #id_or_x{id_or_x = #string{string = "hello"}}}, decode:decode_type(Enc2, {id_or_x, string})),

    RawVal = "hello",
    Enc3 = encode:encode_type(RawVal, {id_or_x, string}),
    ?assertEqual({<<>>, #id_or_x{id_or_x = #string{string = "hello"}}}, decode:decode_type(Enc3, {id_or_x, string})),

    IntVal = {val, 12345},
    Enc4 = encode:encode_type(IntVal, {id_or_x, int}),
    ?assertEqual({<<>>, #id_or_x{id_or_x = #int{int = 12345}}}, decode:decode_type(Enc4, {id_or_x, int})).

id_set_test() ->
    TagVal = "minecraft:planks",
    EncTag = encode:encode_type(TagVal, id_set),
    ?assertEqual({<<>>, #id_set{id_set = TagVal}}, decode:decode_type(EncTag, id_set)),

    IDsVal = [10, 20, 30, 40],
    EncIDs = encode:encode_type(IDsVal, id_set),
    ?assertEqual({<<>>, #id_set{id_set = [10, 20, 30, 40]}}, decode:decode_type(EncIDs, id_set)),

    EmptyIDs = [],
    EncEmpty = encode:encode_type(EmptyIDs, id_set),
    ?assertEqual({<<>>, #id_set{id_set = []}}, decode:decode_type(EncEmpty, id_set)).

array_test() ->
    ListInt = [10, 20, 30, 40],
    EncodedInt = encode:encode_type(ListInt, {array, int}),
    ?assertEqual({<<>>, #array{array = [#int{int = 10}, #int{int = 20}, #int{int = 30}, #int{int = 40}]}}, decode:decode_type(EncodedInt, {array, 4, int})),

    ListStr = ["foo", "bar", "baz"],
    EncodedStr = encode:encode_type(ListStr, {array, string}),
    ?assertEqual({<<>>, #array{array = [#string{string = "foo"}, #string{string = "bar"}, #string{string = "baz"}]}}, decode:decode_type(EncodedStr, {array, 3, string})),

    EmptyList = [],
    EncodedEmpty = encode:encode_type(EmptyList, {array, int}),
    ?assertEqual({<<>>, #array{array = []}}, decode:decode_type(EncodedEmpty, {array, 0, int})).

prefixed_array_test() ->
    ListInt = [10, 20, 30, 40],
    EncodedInt = encode:encode_type(ListInt, {prefixed_array, int}),
    ?assertEqual({<<>>, #prefixed_array{prefixed_array = [#int{int = 10}, #int{int = 20}, #int{int = 30}, #int{int = 40}]}}, decode:decode_type(EncodedInt, {prefixed_array, int})),

    ListStr = ["foo", "bar", "baz"],
    EncodedStr = encode:encode_type(ListStr, {prefixed_array, string}),
    ?assertEqual({<<>>, #prefixed_array{prefixed_array = [#string{string = "foo"}, #string{string = "bar"}, #string{string = "baz"}]}}, decode:decode_type(EncodedStr, {prefixed_array, string})),

    EmptyList = [],
    EncodedEmpty = encode:encode_type(EmptyList, {prefixed_array, int}),
    ?assertEqual({<<>>, #prefixed_array{prefixed_array = []}}, decode:decode_type(EncodedEmpty, {prefixed_array, int})),

    ShortPrefixedList = [100, 200],
    EncodedShortPrefixed = encode:encode_type(ShortPrefixedList, {prefixed_array, short, int}),
    ?assertEqual({<<>>, #prefixed_array{prefixed_array = [#int{int = 100}, #int{int = 200}]}}, decode:decode_type(EncodedShortPrefixed, {prefixed_array, short, int})).

enum_test() ->
    Val = 42,
    ?assertEqual({<<>>, #enum{enum = Val}}, decode:decode_type(encode:encode_type(Val, enum), enum)),
    ?assertEqual({<<>>, #enum{enum = Val}}, decode:decode_type(encode:encode_type(Val, {enum, varint}), {enum, varint})),
    ?assertEqual({<<>>, #enum{enum = Val}}, decode:decode_type(encode:encode_type(Val, {enum, byte}), {enum, byte})),
    ?assertEqual({<<>>, #enum{enum = Val}}, decode:decode_type(encode:encode_type(Val, {enum, int}), {enum, int})),

    EnumList = [north, south, east, west],
    ?assertEqual({<<>>, #enum{enum = south}}, decode:decode_type(encode:encode_type(south, {enum, EnumList}), {enum, EnumList})),
    ?assertEqual({<<>>, #enum{enum = west}}, decode:decode_type(encode:encode_type(west, {enum, byte, EnumList}), {enum, byte, EnumList})).

byte_array_test() ->
    Bytes = <<"some random binary data">>,
    ?assertEqual({<<>>, #byte_array{byte_array = Bytes}}, decode:decode_type(encode:encode_type(Bytes, byte_array), byte_array)),
    ?assertEqual({<<>>, #byte_array{byte_array = Bytes}}, decode:decode_type(encode:encode_type(Bytes, {byte_array, byte_size(Bytes)}), {byte_array, byte_size(Bytes)})),
    ?assertEqual({<<>>, #byte_array{byte_array = Bytes}}, decode:decode_type(encode:encode_type(Bytes, {byte_array, varint}), {byte_array, varint})),
    ?assertEqual({<<>>, #byte_array{byte_array = Bytes}}, decode:decode_type(encode:encode_type(Bytes, {byte_array, short}), {byte_array, short})),
    Str = "string data",
    ?assertEqual({<<>>, #byte_array{byte_array = list_to_binary(Str)}}, decode:decode_type(encode:encode_type(Str, {byte_array, varint}), {byte_array, varint})).

slot_test() ->
    %% Empty slot
    {<<>>, #slot{item_count = 0, itemID = undefined, components_to_add = [], components_to_remove = []}} = decode:decode_type(encode:encode_type(empty, slot), slot),
    {<<>>, #slot{item_count = 0, itemID = undefined, components_to_add = [], components_to_remove = []}} = decode:decode_type(encode:encode_type(#slot{item_count = 0}, slot), slot),
    %% Non-empty, no components
    Slot1 = {3, 42, [], []},
    {<<>>, #slot{item_count = 3, itemID = 42, components_to_add = [], components_to_remove = []}} = decode:decode_type(encode:encode_type(Slot1, slot), slot),
    %% Multiple add and remove components using records
    AddComps = [
        {3, #damage{type = 'minecraft:damage', damage = 15}},
        {4, #unbreakable{type = 'minecraft:unbreakable'}},
        {1, #max_stack_size{type = 'minecraft:max_stack_size', max_stack_size = 16}}
    ],
    RemoveComps = [5, 9],
    Slot2 = #slot{item_count = 1, itemID = 7, components_to_add = AddComps, components_to_remove = RemoveComps},
    {<<>>, Slot2} = decode:decode_type(encode:encode_type(Slot2, slot), slot),
    %% Tuple form input
    Slot3 = {1, 7, AddComps, RemoveComps},
    {<<>>, Slot2} = decode:decode_type(encode:encode_type(Slot3, slot), slot).

hashed_slot_test() ->
    %% Empty hashed slot
    {<<>>, #hashed_slot{item_count = 0, itemID = undefined, components_to_add = [], components_to_remove = []}} = decode:decode_type(encode:encode_type(empty, hashed_slot), hashed_slot),
    %% Non-empty, no components
    HSlot1 = {5, 1, [], []},
    {<<>>, #hashed_slot{item_count = 1, itemID = 5, components_to_add = [], components_to_remove = []}} = decode:decode_type(encode:encode_type(HSlot1, hashed_slot), hashed_slot),
    %% Multiple add and remove components; Hash values include a negative (testing sign)
    HSlot2 = {10, 3, [{2, 99999}, {7, -1}], [4, 8]},
    {<<>>, #hashed_slot{item_count = 3, itemID = 10, components_to_add = [{2, 99999}, {7, -1}], components_to_remove = [4, 8]}} = decode:decode_type(encode:encode_type(HSlot2, hashed_slot), hashed_slot).

sound_event_test() ->
    %% Fixed range present (true)
    Sound1 = {"minecraft:entity.experience_orb.pickup", true, 16.0},
    Encoded1 = encode:encode_type(Sound1, sound_event),
    {<<>>, Decoded1} = decode:decode_type(Encoded1, sound_event),
    ?assertEqual(#sound_event{sound_name = "minecraft:entity.experience_orb.pickup", has_fixed_value = true, fixed_range = 16.0}, Decoded1),

    %% Fixed range absent (false with undefined)
    Sound2 = {"minecraft:entity.pig.ambient", false, undefined},
    Encoded2 = encode:encode_type(Sound2, sound_event),
    {<<>>, Decoded2} = decode:decode_type(Encoded2, sound_event),
    ?assertEqual(#sound_event{sound_name = "minecraft:entity.pig.ambient", has_fixed_value = false, fixed_range = undefined}, Decoded2),

    %% 2-tuple input format {SoundName, false}
    Sound3 = {"minecraft:ui.button.click", false},
    Encoded3 = encode:encode_type(Sound3, sound_event),
    {<<>>, Decoded3} = decode:decode_type(Encoded3, sound_event),
    ?assertEqual(#sound_event{sound_name = "minecraft:ui.button.click", has_fixed_value = false, fixed_range = undefined}, Decoded3).

teleport_flags_test() ->
    %% Test map input
    MapInput = #{
        relative_x => true,
        relative_y => false,
        relative_z => true,
        relative_yaw => false,
        relative_pitch => true,
        relative_velocity_x => false,
        relative_velocity_y => false,
        relative_velocity_z => false,
        rotate_velocity => true
    },
    EncodedMap = encode:encode_type(MapInput, teleport_flags),
    {<<>>, DecodedMap} = decode:decode_type(EncodedMap, teleport_flags),
    ?assertEqual(#teleport_flags{flagsmap = MapInput}, DecodedMap),

    %% Test list input
    ListInput = [relative_x, relative_pitch, rotate_velocity],
    EncodedList = encode:encode_type(ListInput, teleport_flags),
    {<<>>, #teleport_flags{flagsmap = DecodedListMap}} = decode:decode_type(EncodedList, teleport_flags),
    ?assertEqual(true, maps:get(relative_x, DecodedListMap)),
    ?assertEqual(false, maps:get(relative_y, DecodedListMap)),
    ?assertEqual(true, maps:get(relative_pitch, DecodedListMap)),
    ?assertEqual(true, maps:get(rotate_velocity, DecodedListMap)),

    %% Test integer bitfield input
    IntInput = 16#0105,
    EncodedInt = encode:encode_type(IntInput, teleport_flags),
    {<<>>, #teleport_flags{flagsmap = DecodedIntMap}} = decode:decode_type(EncodedInt, teleport_flags),
    ?assertEqual(true, maps:get(relative_x, DecodedIntMap)),
    ?assertEqual(true, maps:get(relative_z, DecodedIntMap)),
    ?assertEqual(true, maps:get(rotate_velocity, DecodedIntMap)).

slot_display_test() ->
    %% 0. empty
    Empty = #empty{type = 'minecraft:empty'},
    ?assertEqual({<<>>, Empty}, decode:decode_type(encode:encode_type(Empty, slot_display), slot_display)),

    %% 1. any_fuel
    Fuel = #any_fuel{type = 'minecraft:any_fuel'},
    ?assertEqual({<<>>, Fuel}, decode:decode_type(encode:encode_type(Fuel, slot_display), slot_display)),

    %% 2. with_any_potion
    Potion = #with_any_potion{type = 'minecraft:with_any_potion', base = #item{type = 'minecraft:item', item_type = 10}},
    ?assertEqual({<<>>, Potion}, decode:decode_type(encode:encode_type(Potion, slot_display), slot_display)),

    %% 3. only_with_component
    Comp = #only_with_component{type = 'minecraft:only_with_component', base = #item{type = 'minecraft:item', item_type = 1}, component_type_id = 5},
    ?assertEqual({<<>>, Comp}, decode:decode_type(encode:encode_type(Comp, slot_display), slot_display)),

    %% 4. item
    Item = #item{type = 'minecraft:item', item_type = 42},
    ?assertEqual({<<>>, Item}, decode:decode_type(encode:encode_type(Item, slot_display), slot_display)),

    %% 5. item_stack
    SlotVal = {1, 20, [], []},
    ItemStack = #item_stack{type = 'minecraft:item_stack', item_stack = SlotVal},
    ExpectedItemStack = #item_stack{type = 'minecraft:item_stack', item_stack = #slot{item_count = 1, itemID = 20, components_to_add = [], components_to_remove = []}},
    ?assertEqual({<<>>, ExpectedItemStack}, decode:decode_type(encode:encode_type(ItemStack, slot_display), slot_display)),

    %% 6. tag
    Tag = #tag{type = 'minecraft:tag', tag = "minecraft:planks"},
    ?assertEqual({<<>>, Tag}, decode:decode_type(encode:encode_type(Tag, slot_display), slot_display)),

    %% 7. dyed
    Dyed = #dyed{type = 'minecraft:dyed', dye = #item{type = 'minecraft:item', item_type = 2}, target = #item{type = 'minecraft:item', item_type = 3}},
    ?assertEqual({<<>>, Dyed}, decode:decode_type(encode:encode_type(Dyed, slot_display), slot_display)),

    %% 8. smithing_trim
    Trim = #smithing_trim{type = 'minecraft:smithing_trim', base = Item, material = #item{type = 'minecraft:item', item_type = 4}, pattern = 100},
    ?assertEqual({<<>>, Trim}, decode:decode_type(encode:encode_type(Trim, slot_display), slot_display)),

    %% 9. with_remainder
    Rem = #with_remainder{type = 'minecraft:with_remainder', ingredient = Item, remainder = Empty},
    ?assertEqual({<<>>, Rem}, decode:decode_type(encode:encode_type(Rem, slot_display), slot_display)),

    %% 10. composite
    CompDisplay = #composite{type = 'minecraft:composite', options_count = 2, options = [Item, Tag]},
    ?assertEqual({<<>>, CompDisplay}, decode:decode_type(encode:encode_type(CompDisplay, slot_display), slot_display)).

recipe_display_test() ->
    EmptySlot = #empty{type = 'minecraft:empty'},
    ItemSlot = #item{type = 'minecraft:item', item_type = 10},

    Shapeless = #crafting_shapeless{
        type = 'minecraft:crafting_shapeless',
        ingredients_count = 1,
        ingredients = [ItemSlot],
        result = EmptySlot,
        crafting_station = EmptySlot
    },
    ?assertEqual({<<>>, Shapeless}, decode:decode_type(encode:encode_type(Shapeless, recipe_display), recipe_display)),

    Shaped = #crafting_shaped{
        type = 'minecraft:crafting_shaped',
        width = 2,
        height = 2,
        ingredients_count = 4,
        ingredients = [ItemSlot, ItemSlot, ItemSlot, ItemSlot],
        result = EmptySlot,
        crafting_station = EmptySlot
    },
    ?assertEqual({<<>>, Shaped}, decode:decode_type(encode:encode_type(Shaped, recipe_display), recipe_display)),

    Furnace = #furnace{
        type = 'minecraft:furnace',
        ingredient = ItemSlot,
        fuel = EmptySlot,
        result = EmptySlot,
        crafting_station = EmptySlot,
        cooking_time = 200,
        experience = 0.35
    },
    {<<>>, DecodedFurnace} = decode:decode_type(encode:encode_type(Furnace, recipe_display), recipe_display),
    ?assertEqual(Furnace#furnace.type, DecodedFurnace#furnace.type),
    ?assertEqual(Furnace#furnace.ingredient, DecodedFurnace#furnace.ingredient),
    ?assertEqual(Furnace#furnace.fuel, DecodedFurnace#furnace.fuel),
    ?assertEqual(Furnace#furnace.result, DecodedFurnace#furnace.result),
    ?assertEqual(Furnace#furnace.crafting_station, DecodedFurnace#furnace.crafting_station),
    ?assertEqual(Furnace#furnace.cooking_time, DecodedFurnace#furnace.cooking_time),
    ?assert(abs(Furnace#furnace.experience - DecodedFurnace#furnace.experience) < 0.0001),

    Stonecutter = #stonecutter{
        type = 'minecraft:stonecutter',
        ingredient = ItemSlot,
        result = EmptySlot,
        crafting_station = EmptySlot
    },
    ?assertEqual({<<>>, Stonecutter}, decode:decode_type(encode:encode_type(Stonecutter, recipe_display), recipe_display)),

    Smithing = #smithing{
        type = 'minecraft:smithing',
        template = ItemSlot,
        base = ItemSlot,
        addition = ItemSlot,
        result = EmptySlot,
        crafting_station = EmptySlot
    },
    ?assertEqual({<<>>, Smithing}, decode:decode_type(encode:encode_type(Smithing, recipe_display), recipe_display)).

either_x_or_y_test() ->
    TypeSpec = {either_x_or_y, byte, string},
    ValX = {x, 123},
    ValY = {y, "test_string"},
    EncodedX = encode:encode_type(ValX, TypeSpec),
    EncodedY = encode:encode_type(ValY, TypeSpec),
    ?assertEqual({<<>>, #either_x_or_y{x = #byte{byte = 123}, y = undefined}}, decode:decode_type(EncodedX, TypeSpec)),
    ?assertEqual({<<>>, #either_x_or_y{x = undefined, y = #string{string = "test_string"}}}, decode:decode_type(EncodedY, TypeSpec)).

game_profile_test() ->
    UUID = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    Username = "Steve",
    Properties = [
        {"textures", "base64_texture_data", {some, "sig_abc"}},
        {"cape", "cape_data", none}
    ],
    Profile = {UUID, Username, Properties},
    Encoded = encode:encode_type(Profile, game_profile),
    Expected = #game_profile{
        uuid = UUID,
        username = Username,
        properties = [
            {"textures", "base64_texture_data", #prefixed_optional{some = some, prefixed_optional = #string{string = "sig_abc"}}},
            {"cape", "cape_data", #prefixed_optional{some = none, prefixed_optional = none}}
        ]
    },
    ?assertEqual({<<>>, Expected}, decode:decode_type(Encoded, game_profile)).

resolvable_profile_roundtrip_test() ->
    UUID = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    %% Test Partial (ProfileKind = 0)
    PartialData = {0, {{some, "Steve"}, {some, UUID}, [{"textures", "base64_val", {some, "sig123"}}]}, {some, "minecraft:textures/skin1"}, none, none, {some, 0}},
    TypeSpecPartial = {resolvable_profile, true, false, false, true},
    EncodedPartial = encode:encode_type(PartialData, TypeSpecPartial),
    ExpectedPartialProfile = {#prefixed_optional{some = some, prefixed_optional = #string{string = "Steve"}}, #prefixed_optional{some = some, prefixed_optional = #uuid{uuid = UUID}}, [{"textures", "base64_val", #prefixed_optional{some = some, prefixed_optional = #string{string = "sig123"}}}]},
    ExpectedPartial = #resolvable_profile{profile_kind = 0, profile = ExpectedPartialProfile, body = #optional{some = some, optional = #identifier{identifier = "minecraft:textures/skin1"}}, cape = #optional{some = none, optional = none}, elytra = #optional{some = none, optional = none}, model = #optional{some = some, optional = #varint{varint = 0}}},
    ?assertEqual({<<>>, ExpectedPartial}, decode:decode_type(EncodedPartial, TypeSpecPartial)),

    %% Test Complete (ProfileKind = 1)
    GameProfile = {UUID, "Alex", [{"textures", "val2", none}]},
    CompleteData = {1, GameProfile, none, {some, "minecraft:textures/cape1"}, none, none},
    TypeSpecComplete = {resolvable_profile, false, true, false, false},
    EncodedComplete = encode:encode_type(CompleteData, TypeSpecComplete),
    ExpectedCompleteProfile = #game_profile{uuid = UUID, username = "Alex", properties = [{"textures", "val2", #prefixed_optional{some = none, prefixed_optional = none}}]},
    ExpectedComplete = #resolvable_profile{profile_kind = 1, profile = ExpectedCompleteProfile, body = #optional{some = none, optional = none}, cape = #optional{some = some, optional = #identifier{identifier = "minecraft:textures/cape1"}}, elytra = #optional{some = none, optional = none}, model = #optional{some = none, optional = none}},
    ?assertEqual({<<>>, ExpectedComplete}, decode:decode_type(EncodedComplete, TypeSpecComplete)).

debug_subscription_event_test() ->
    Records = [
        #dedicated_server_tick_time{type = dedicated_server_tick_time},
        #bee{type = bee, hive_position = #prefixed_optional{some = some, prefixed_optional = #position{x=1,y=2,z=3}}, flower_position = #prefixed_optional{some = none, prefixed_optional = none}, travel_ticks = 100, blacklisted_hives = [#position{x=4,y=5,z=6}]},
        #villager_brain{type = villager_brain, name = "villager", profession = "farmer", xp = 10, health = 20.0, max_health = 20.0, inventory = "", wants_golem = false, anger_level = 0, activities = ["work"], behaviors = [], memories = [], gossips = [], pois = [#position{x=1,y=2,z=3}], potential_pois = []},
        #breeze{type = breeze, attack_target = #prefixed_optional{some = some, prefixed_optional = 5}, jump_target = #prefixed_optional{some = none, prefixed_optional = none}},
        #goal_selector{type = goal_selector, priority = 1, is_running = true, name = "swim"},
        #entity_path{type = entity_path, reached = true, next_block_index = 10, block_position = #position{x=10,y=20,z=30}, nodes = [{1,2,3, 1.0, 0.5, true, 0, 1}], target_nodes = [], open_set = [], closed_set = [], max_node_distance = 2.5},
        #entity_block_intersection{type = entity_block_intersection, id = 2},
        #bee_hive{type = bee_hive, hive_type = 5, occupant_count = 3, honey_level = 4, sedated = true},
        #poi{type = poi, position = #position{x=10,y=20,z=30}, poi_type = 1, free_ticket_count = 2},
        #redstone_wire_orientation{type = redstone_wire_orientation, id = 42},
        #village_section{type = village_section},
        #raid{type = raid, positions = [#position{x=1,y=1,z=1}, #position{x=2,y=2,z=2}]},
        #structure{type = structure, structures = [{{0,0,0,20,20,20}, [{{0,0,0,10,10,10}, true}]}]},
        #game_event_listener{type = game_event_listener, listener_radius = 8},
        #neighbor_update{type = neighbor_update, position = #position{x=7,y=8,z=9}},
        #game_event{type = game_event, event = 3, x = 1.5, y = 2.5, z = 3.5}
    ],
    lists:foreach(fun(Rec) ->
        Enc = encode:encode_type(Rec, debug_subscription_event),
        {<<>>, Dec} = decode:decode_type(Enc, debug_subscription_event),
        ?assertMatch(#debug_subscription_event{}, Dec),

        Enc2 = encode:encode_type(Dec, debug_subscription_event),
        {<<>>, Dec2} = decode:decode_type(Enc2, debug_subscription_event),
        ?assertEqual(Dec, Dec2)
    end, Records).

debug_subscription_update_test() ->
    Bee = #bee{type = bee, hive_position = {some, {1,2,3}}, flower_position = none, travel_ticks = 100, blacklisted_hives = [{4,5,6}]},
    %% Present update (using {some, Record})
    EncPresent = encode:encode_type({some, Bee}, debug_subscription_update),
    {<<>>, #prefixed_optional{some = some, prefixed_optional = #bee{}}} = decode:decode_type(EncPresent, debug_subscription_update),

    %% Present update (using {Type, {some, Record}})
    EncPresent2 = encode:encode_type({1, {some, Bee}}, debug_subscription_update),
    {<<>>, #prefixed_optional{some = some, prefixed_optional = #bee{}}} = decode:decode_type(EncPresent2, debug_subscription_update),

    %% Absent update (using {Type, none})
    EncAbsent = encode:encode_type({1, none}, debug_subscription_update),
    {<<>>, #prefixed_optional{some = none, prefixed_optional = none}} = decode:decode_type(EncAbsent, debug_subscription_update).

lp_vec3_test() ->
    TestVecs = [
        #lp_vec3{x = 0.0, y = 0.0, z = 0.0},
        #lp_vec3{x = 1.0, y = 0.0, z = -1.0},
        #lp_vec3{x = 10.0, y = 0.2, z = -5.0},
        #lp_vec3{x = 123457.0, y = 15.071, z = 0.0}
    ],
    lists:foreach(fun(#lp_vec3{x = X, y = Y, z = Z} = Vec) ->
        Encoded = encode:encode_type(Vec, lp_vec3),
        {<<>>, #lp_vec3{x = DecX, y = DecY, z = DecZ}} = decode:decode_type(Encoded, lp_vec3),
        ?assert(abs(X - DecX) < 0.05),
        ?assert(abs(Y - DecY) < 0.05),
        ?assert(abs(Z - DecZ) < 0.05),
        Encoded2 = encode:encode_type(#lp_vec3{x = DecX, y = DecY, z = DecZ}, lp_vec3),
        ?assertEqual(Encoded, Encoded2)
    end, TestVecs).

seen_advancements_test() ->
    Rec0 = #seen_advancements{action = 0, tab_id = "minecraft:story/root"},
    Enc0 = encode:encode_type(Rec0, seen_advancements),
    {<<>>, Dec0} = decode:decode_type(Enc0, seen_advancements),
    Enc0_re = encode:encode_type(Dec0, seen_advancements),
    ?assertEqual(Enc0, Enc0_re),

    Rec1 = #seen_advancements{action = 1, tab_id = none},
    Enc1 = encode:encode_type(Rec1, seen_advancements),
    {<<>>, Dec1} = decode:decode_type(Enc1, seen_advancements),
    Enc1_re = encode:encode_type(Dec1, seen_advancements),
    ?assertEqual(Enc1, Enc1_re).

boss_bar_test() ->
    UUID = <<"1234567890123456">>,
    BossBars = [
        #boss_bar{uuid = UUID, action = 0, title = "Ender Dragon", health = 0.75, color = 1, division = 2, flags = 5},
        #boss_bar{uuid = UUID, action = 1},
        #boss_bar{uuid = UUID, action = 2, health = 0.5},
        #boss_bar{uuid = UUID, action = 3, title = "Wither"},
        #boss_bar{uuid = UUID, action = 4, color = 3, division = 1},
        #boss_bar{uuid = UUID, action = 5, flags = 3}
    ],
    lists:foreach(fun(Rec) ->
        Enc = encode:encode_type(Rec, boss_bar),
        {<<>>, Dec} = decode:decode_type(Enc, boss_bar),
        Enc_re = encode:encode_type(Dec, boss_bar),
        {<<>>, Dec_re} = decode:decode_type(Enc_re, boss_bar),
        ?assertEqual(Dec, Dec_re)
    end, BossBars).

node_test() ->
    Nodes = [
        %% Root Node
        #node{flags = 0, children = [1, 2]},
        %% Literal Node
        #node{flags = 1, children = [3], name = "gamemode"},
        %% Argument Node with Entity parser (ID 6)
        #node{flags = 2, children = [], name = "target", parser_id = 6, properties = 1},
        %% Argument Node with Float parser (ID 1) and Suggestions Type
        #node{flags = 18, children = [], name = "amount", parser_id = 1, properties = {3, 0.0, 100.0}, suggestions_type = "minecraft:ask_server"},
        %% Argument Node with Double parser (ID 2)
        #node{flags = 2, children = [], name = "val", parser_id = 2, properties = {1, 1.5, none}},
        %% Argument Node with Integer parser (ID 3)
        #node{flags = 2, children = [], name = "count", parser_id = 3, properties = {2, none, 10}},
        %% Argument Node with Long parser (ID 4)
        #node{flags = 2, children = [], name = "ticks", parser_id = 4, properties = {0, none, none}},
        %% Argument Node with String parser (ID 5)
        #node{flags = 2, children = [], name = "msg", parser_id = 5, properties = 2},
        %% Argument Node with Time parser (ID 23)
        #node{flags = 2, children = [], name = "duration", parser_id = 23, properties = 20},
        %% Argument Node with Resource tag parser (ID 24)
        #node{flags = 2, children = [], name = "item", parser_id = 24, properties = "minecraft:item"},
        %% Argument Node with Score Holder parser (ID 34)
        #node{flags = 2, children = [], name = "objective", parser_id = 34, properties = 0},
        %% Redirect Node
        #node{flags = 8, children = [], redirect_node = 4},
        %% Restricted Executable Literal Node (flags: 0x01 | 0x04 | 0x20 = 37)
        #node{flags = 37, children = [], name = "admin"}
    ],
    lists:foreach(fun(Rec) ->
        Enc = encode:encode_type(Rec, node),
        {<<>>, Dec} = decode:decode_type(Enc, node),
        Enc_re = encode:encode_type(Dec, node),
        {<<>>, Dec_re} = decode:decode_type(Enc_re, node),
        ?assertEqual(Enc, Enc_re),
        ?assertEqual(Dec, Dec_re)
    end, Nodes).


delete_chat_test() ->
    Sig256 = binary:copy(<<123:8>>, 256),

    %% Case 1: Message ID = 0, Signature is present (256 bytes)
    Rec0 = #delete_chat{message_id = 0, signature = Sig256},
    Enc0 = encode:encode_type(Rec0, delete_chat),
    ?assertEqual(257, byte_size(Enc0)),
    {<<>>, Dec0} = decode:decode_type(Enc0, delete_chat),
    ?assertEqual(#varint{varint = 0}, Dec0#delete_chat.message_id),
    ?assertEqual(#optional{some = some, optional = #byte_array{byte_array = Sig256}}, Dec0#delete_chat.signature),

    %% Re-encode decoded record
    Enc0_re = encode:encode_type(Dec0, delete_chat),
    ?assertEqual(Enc0, Enc0_re),
    {<<>>, Dec0_re} = decode:decode_type(Enc0_re, delete_chat),
    ?assertEqual(Dec0, Dec0_re),

    %% Case 2: Message ID != 0 (e.g. 5), Signature is not present
    Rec1 = #delete_chat{message_id = 5, signature = none},
    Enc1 = encode:encode_type(Rec1, delete_chat),
    ?assertEqual(1, byte_size(Enc1)),
    {<<>>, Dec1} = decode:decode_type(Enc1, delete_chat),
    ?assertEqual(#varint{varint = 5}, Dec1#delete_chat.message_id),
    ?assertEqual(#optional{some = none, optional = none}, Dec1#delete_chat.signature),

    %% Re-encode decoded record
    Enc1_re = encode:encode_type(Dec1, delete_chat),
    ?assertEqual(Enc1, Enc1_re),
    {<<>>, Dec1_re} = decode:decode_type(Enc1_re, delete_chat),
    ?assertEqual(Dec1, Dec1_re).

chat_type_test() ->
    NbtVal = [{tag_compound, "", []}],
    Rec0 = #chat_type{
        translation_key = "chat.type.text",
        parameters = [sender, content],
        style = NbtVal
    },
    Enc0 = encode:encode_type(Rec0, chat_type),
    {<<>>, Dec0} = decode:decode_type(Enc0, chat_type),
    ?assertEqual(#string{string = "chat.type.text"}, Dec0#chat_type.translation_key),
    ?assertEqual(#prefixed_array{prefixed_array = [#enum{enum = sender}, #enum{enum = content}]}, Dec0#chat_type.parameters),
    ?assertEqual(#nbt{nbt = NbtVal}, Dec0#chat_type.style),

    %% Re-encode decoded record
    Enc0_re = encode:encode_type(Dec0, chat_type),
    ?assertEqual(Enc0, Enc0_re),
    {<<>>, Dec0_re} = decode:decode_type(Enc0_re, chat_type),
    ?assertEqual(Dec0, Dec0_re),

    %% Case with tuple input and all 3 parameters
    Rec1 = {"chat.type.emote", [sender, target, content], NbtVal},
    Enc1 = encode:encode_type(Rec1, chat_type),
    {<<>>, Dec1} = decode:decode_type(Enc1, chat_type),
    ?assertEqual(#string{string = "chat.type.emote"}, Dec1#chat_type.translation_key),
    ?assertEqual(#prefixed_array{prefixed_array = [#enum{enum = sender}, #enum{enum = target}, #enum{enum = content}]}, Dec1#chat_type.parameters),
    ?assertEqual(#nbt{nbt = NbtVal}, Dec1#chat_type.style).

player_info_update_test() ->
    UUID1 = <<"1234567890123456">>,
    UUID2 = <<"6543210987654321">>,

    %% 1. Single action: Add Player (0x01)
    AddPlayerAction = {add_player, "PlayerOne", [{"textures", "val1", "sig1"}]},
    Rec1 = #player_info_update{
        actions = 16#01,
        players = [
            #player_info_entry{uuid = UUID1, actions = [AddPlayerAction]}
        ]
    },
    Enc1 = encode:encode_type(Rec1, player_info_update),
    {<<>>, Dec1} = decode:decode_type(Enc1, player_info_update),
    Enc1_re = encode:encode_type(Dec1, player_info_update),
    {<<>>, Dec1_re} = decode:decode_type(Enc1_re, player_info_update),
    ?assertEqual(Enc1, Enc1_re),
    ?assertEqual(Dec1, Dec1_re),

    %% 2. Single action: Initialize Chat (0x02) with signature data
    ChatSessionSig = {UUID1, 1234567890, <<"pubkey">>, <<"keysig">>},
    InitChatAction = {initialize_chat, {some, ChatSessionSig}},
    Rec2 = #player_info_update{
        actions = 16#02,
        players = [#player_info_entry{uuid = UUID1, actions = [InitChatAction]}]
    },
    Enc2 = encode:encode_type(Rec2, player_info_update),
    {<<>>, Dec2} = decode:decode_type(Enc2, player_info_update),
    Enc2_re = encode:encode_type(Dec2, player_info_update),
    {<<>>, Dec2_re} = decode:decode_type(Enc2_re, player_info_update),
    ?assertEqual(Enc2, Enc2_re),
    ?assertEqual(Dec2, Dec2_re),

    %% 3. Single action: Update Game Mode (0x04)
    GameModeAction = {update_game_mode, 1}, % Creative
    Rec3 = #player_info_update{
        actions = 16#04,
        players = [#player_info_entry{uuid = UUID1, actions = [GameModeAction]}]
    },
    Enc3 = encode:encode_type(Rec3, player_info_update),
    {<<>>, Dec3} = decode:decode_type(Enc3, player_info_update),
    Enc3_re = encode:encode_type(Dec3, player_info_update),
    {<<>>, Dec3_re} = decode:decode_type(Enc3_re, player_info_update),
    ?assertEqual(Enc3, Enc3_re),
    ?assertEqual(Dec3, Dec3_re),

    %% 4. Single action: Update Listed (0x08)
    ListedAction = {update_listed, true},
    Rec4 = #player_info_update{
        actions = 16#08,
        players = [#player_info_entry{uuid = UUID1, actions = [ListedAction]}]
    },
    Enc4 = encode:encode_type(Rec4, player_info_update),
    {<<>>, Dec4} = decode:decode_type(Enc4, player_info_update),
    Enc4_re = encode:encode_type(Dec4, player_info_update),
    {<<>>, Dec4_re} = decode:decode_type(Enc4_re, player_info_update),
    ?assertEqual(Enc4, Enc4_re),
    ?assertEqual(Dec4, Dec4_re),

    %% 5. Single action: Update Latency (0x10)
    LatencyAction = {update_latency, 42},
    Rec5 = #player_info_update{
        actions = 16#10,
        players = [#player_info_entry{uuid = UUID1, actions = [LatencyAction]}]
    },
    Enc5 = encode:encode_type(Rec5, player_info_update),
    {<<>>, Dec5} = decode:decode_type(Enc5, player_info_update),
    Enc5_re = encode:encode_type(Dec5, player_info_update),
    {<<>>, Dec5_re} = decode:decode_type(Enc5_re, player_info_update),
    ?assertEqual(Enc5, Enc5_re),
    ?assertEqual(Dec5, Dec5_re),

    %% 6. Single action: Update Display Name (0x20)
    DisplayNameAction = {update_display_name, {some, #{<<"text">> => <<"PlayerOne">>}}},
    Rec6 = #player_info_update{
        actions = 16#20,
        players = [#player_info_entry{uuid = UUID1, actions = [DisplayNameAction]}]
    },
    Enc6 = encode:encode_type(Rec6, player_info_update),
    {<<>>, Dec6} = decode:decode_type(Enc6, player_info_update),
    Enc6_re = encode:encode_type(Dec6, player_info_update),
    {<<>>, Dec6_re} = decode:decode_type(Enc6_re, player_info_update),
    ?assertEqual(Enc6, Enc6_re),
    ?assertEqual(Dec6, Dec6_re),

    %% 7. Combined multiple actions (0x01 | 0x04 | 0x08 | 0x10 | 0x20 = 0x3D) and multiple players
    CombinedActions = 16#01 bor 16#04 bor 16#08 bor 16#10 bor 16#20,
    Player1Actions = [
        {add_player, "Player1", []},
        {update_game_mode, 0},
        {update_listed, true},
        {update_latency, 20},
        {update_display_name, none}
    ],
    Player2Actions = [
        {add_player, "Player2", [{"skin", "val2", none}]},
        {update_game_mode, 2},
        {update_listed, false},
        {update_latency, 100},
        {update_display_name, {some, #{<<"text">> => <<"P2">>}}}
    ],
    Rec7 = #player_info_update{
        actions = CombinedActions,
        players = [
            #player_info_entry{uuid = UUID1, actions = Player1Actions},
            #player_info_entry{uuid = UUID2, actions = Player2Actions}
        ]
    },
    Enc7 = encode:encode_type(Rec7, player_info_update),
    {<<>>, Dec7} = decode:decode_type(Enc7, player_info_update),
    Enc7_re = encode:encode_type(Dec7, player_info_update),
    {<<>>, Dec7_re} = decode:decode_type(Enc7_re, player_info_update),
    ?assertEqual(Enc7, Enc7_re),
    ?assertEqual(Dec7, Dec7_re).

set_equipment_test() ->
    EmptySlot = #slot{item_count = 0, itemID = undefined, components_to_add = [], components_to_remove = []},
    ItemSlot = #slot{item_count = 1, itemID = 267, components_to_add = [], components_to_remove = []},
    BootsSlot = #slot{item_count = 1, itemID = 313, components_to_add = [], components_to_remove = []},

    %% 1. Single equipment item test
    Rec1 = #set_equipment{
        entity_id = 123,
        equipment = [
            {0, EmptySlot}
        ]
    },
    Enc1 = encode:encode_type(Rec1, set_equipment),
    {<<>>, Dec1} = decode:decode_type(Enc1, set_equipment),
    ?assertEqual(#varint{varint = 123}, Dec1#set_equipment.entity_id),
    ?assertEqual([{#enum{enum = 0}, EmptySlot}], Dec1#set_equipment.equipment),
    Enc1_re = encode:encode_type(Dec1, set_equipment),
    ?assertEqual(Enc1, Enc1_re),

    %% 2. Multiple equipment items test
    Rec2 = #set_equipment{
        entity_id = 456,
        equipment = [
            {#enum{enum = 0}, ItemSlot},
            {#byte{byte = 2}, BootsSlot},
            {5, EmptySlot}
        ]
    },
    Enc2 = encode:encode_type(Rec2, set_equipment),
    {<<>>, Dec2} = decode:decode_type(Enc2, set_equipment),
    ?assertEqual(#varint{varint = 456}, Dec2#set_equipment.entity_id),
    ?assertEqual([
        {#enum{enum = 0}, ItemSlot},
        {#enum{enum = 2}, BootsSlot},
        {#enum{enum = 5}, EmptySlot}
    ], Dec2#set_equipment.equipment),
    Enc2_re = encode:encode_type(Dec2, set_equipment),
    ?assertEqual(Enc2, Enc2_re),

    %% 3. Tuple input test
    TupleRec = {789, [{0, ItemSlot}, {1, EmptySlot}]},
    Enc3 = encode:encode_type(TupleRec, set_equipment),
    {<<>>, Dec3} = decode:decode_type(Enc3, set_equipment),
    ?assertEqual(#varint{varint = 789}, Dec3#set_equipment.entity_id),
    ?assertEqual([
        {#enum{enum = 0}, ItemSlot},
        {#enum{enum = 1}, EmptySlot}
    ], Dec3#set_equipment.equipment),
    Enc3_re = encode:encode_type(Dec3, set_equipment),
    ?assertEqual(Enc3, Enc3_re).

set_objective_test() ->
    TextVal = #{type => <<"text">>, text => <<"Scores">>},
    FixedVal = #{type => <<"text">>, text => <<"100">>},
    StylingTag = [{tag_compound, "", [{tag_string, "color", "red"}, {tag_byte, "bold", 1}]}],

    Objectives = [
        #set_objective{
            objective_name = "obj_remove",
            mode = 1
        },
        #set_objective{
            objective_name = "obj_no_fmt",
            mode = 0,
            objective_value = TextVal,
            type = 0,
            number_format = undefined
        },
        #set_objective{
            objective_name = "obj_blank_fmt",
            mode = 0,
            objective_value = TextVal,
            type = 1,
            number_format = blank
        },
        #set_objective{
            objective_name = "obj_styled_fmt",
            mode = 0,
            objective_value = TextVal,
            type = 0,
            number_format = {styled, StylingTag}
        },
        #set_objective{
            objective_name = "obj_fixed_fmt",
            mode = 2,
            objective_value = TextVal,
            type = 1,
            number_format = {fixed, FixedVal}
        }
    ],

    lists:foreach(fun(Rec) ->
        Enc = encode:encode_type(Rec, set_objective),
        {<<>>, Dec} = decode:decode_type(Enc, set_objective),
        Enc_re = encode:encode_type(Dec, set_objective),
        {<<>>, Dec_re} = decode:decode_type(Enc_re, set_objective),
        ?assertEqual(Dec, Dec_re),
        ?assertEqual(Enc, Enc_re)
    end, Objectives).

set_player_team_test() ->
    TextVal1 = #{type => <<"text">>, text => <<"Team Gold">>},
    PrefixVal = #{type => <<"text">>, text => <<"[Gold] ">>},
    SuffixVal = #{type => <<"text">>, text => <<" [VIP]">>},

    Teams = [
        #set_player_team{
            team_name = "gold_team",
            method = 1
        },
        #set_player_team{
            team_name = "gold_team",
            method = 0,
            team_display_name = TextVal1,
            team_prefix = PrefixVal,
            team_suffix = SuffixVal,
            name_tag_visibility = 0,
            collision_rule = 3,
            team_color = 6,
            friendly_flags = 3,
            entities = #prefixed_array{prefixed_array = [#string{string = "Alice"}, #string{string = "Bob"}]}
        },
        #set_player_team{
            team_name = "gold_team",
            method = 2,
            team_display_name = TextVal1,
            team_prefix = PrefixVal,
            team_suffix = SuffixVal,
            name_tag_visibility = 2,
            collision_rule = 1,
            team_color = 14,
            friendly_flags = 0
        },
        #set_player_team{
            team_name = "gold_team",
            method = 3,
            entities = #prefixed_array{prefixed_array = [#string{string = "Charlie"}]}
        },
        #set_player_team{
            team_name = "gold_team",
            method = 4,
            entities = #prefixed_array{prefixed_array = [#string{string = "Alice"}]}
        }
    ],

    lists:foreach(fun(Rec) ->
        Enc = encode:encode_type(Rec, set_player_team),
        {<<>>, Dec} = decode:decode_type(Enc, set_player_team),
        Enc_re = encode:encode_type(Dec, set_player_team),
        {<<>>, Dec_re} = decode:decode_type(Enc_re, set_player_team),
        ?assertEqual(Dec, Dec_re),
        ?assertEqual(Enc, Enc_re)
    end, Teams).

waypoint_data_test() ->
    Waypoints = [
        #waypoint_data{
            waypoint_type = 0
        },
        #waypoint_data{
            waypoint_type = 1,
            x = 500,
            y = 70,
            z = -300
        },
        #waypoint_data{
            waypoint_type = 2,
            x = -32,
            z = 64
        },
        #waypoint_data{
            waypoint_type = 3,
            angle = 2.71828
        }
    ],

    lists:foreach(fun(Rec) ->
        Enc = encode:encode_type(Rec, waypoint_data),
        {<<>>, Dec} = decode:decode_type(Enc, waypoint_data),
        Enc_re = encode:encode_type(Dec, waypoint_data),
        {<<>>, Dec_re} = decode:decode_type(Enc_re, waypoint_data),
        ?assertEqual(Dec, Dec_re),
        ?assertEqual(Enc, Enc_re)
    end, Waypoints).

stop_sound_test() ->
    Sounds = [
        #stop_sound{
            flags = 0
        },
        #stop_sound{
            source = master
        },
        #stop_sound{
            sound = <<"minecraft:ambient.cave">>
        },
        #stop_sound{
            source = player,
            sound = <<"minecraft:entity.generic.explode">>
        }
    ],

    lists:foreach(fun(Rec) ->
        Enc = encode:encode_type(Rec, stop_sound),
        {<<>>, Dec} = decode:decode_type(Enc, stop_sound),
        Enc_re = encode:encode_type(Dec, stop_sound),
        {<<>>, Dec_re} = decode:decode_type(Enc_re, stop_sound),
        ?assertEqual(Dec, Dec_re),
        ?assertEqual(Enc, Enc_re)
    end, Sounds).

set_score_test() ->
    Scores = [
        #set_score{
            entity_name = "Steve",
            objective_name = "points",
            value = 10
        },
        #set_score{
            entity_name = "Alex",
            objective_name = "points",
            value = 20,
            display_name = #{type => <<"text">>, text => <<"Alex Score">>},
            number_format = blank
        },
        #set_score{
            entity_name = "Player3",
            objective_name = "health",
            value = 100,
            display_name = undefined,
            number_format = {styled, [{tag_compound, "", [{tag_string, "color", "blue"}]}]}
        },
        #set_score{
            entity_name = "Player4",
            objective_name = "coins",
            value = 50,
            display_name = #{type => <<"text">>, text => <<"Coins">>},
            number_format = {fixed, #{type => <<"text">>, text => <<"$$$">>}}
        }
    ],

    lists:foreach(fun(Rec) ->
        Enc = encode:encode_type(Rec, set_score),
        {<<>>, Dec} = decode:decode_type(Enc, set_score),
        Enc_re = encode:encode_type(Dec, set_score),
        {<<>>, Dec_re} = decode:decode_type(Enc_re, set_score),
        ?assertEqual(Dec, Dec_re),
        ?assertEqual(Enc, Enc_re)
    end, Scores).

%% -----------------------------------------------------------------
%% update_advancements tests
%% -----------------------------------------------------------------

update_advancements_test() ->
    %% Helper: round-trip: encode→decode→encode→decode and compare stability of decoded records.
    %% We compare Dec == Dec2 (second round-trip is stable) rather than Enc == Enc2, because
    %% text_component encoding converts JSON keys to atom keys on decode, so the first
    %% re-encode may produce SNBT rather than JSON (semantically equivalent but different bytes).
    RoundTrip = fun(Rec) ->
        Enc  = encode:encode_type(Rec, update_advancements),
        {<<>>, Dec}  = decode:decode_type(Enc, update_advancements),
        Enc2 = encode:encode_type(Dec, update_advancements),
        {<<>>, Dec2} = decode:decode_type(Enc2, update_advancements),
        Enc3 = encode:encode_type(Dec2, update_advancements),
        ?assertEqual(Enc2, Enc3),
        ?assertEqual(Dec,  Dec2)
    end,

    %% Stable text components (NBT maps re-encode identically)
    TitleTC   = #{<<"text">> => <<"Mine Stone">>},
    DescTC    = #{<<"text">> => <<"Mine stone using a pickaxe">>},
    RootTC    = #{<<"text">> => <<"Root">>},
    WelcomeTC = #{<<"text">> => <<"Welcome">>},
    ChalTC    = #{<<"text">> => <<"Challenge">>},
    DragonTC  = #{<<"text">> => <<"Defeat the Ender Dragon">>},

    %% Case 1: Reset=true, no advancements, no identifiers, no progress
    Rec1 = #update_advancements{
        reset               = true,
        advancement_mapping = [],
        identifiers         = [],
        progress_mapping    = []
    },
    RoundTrip(Rec1),

    %% Case 2: Reset=false, one advancement without display data, no progress
    Adv2 = #advancement{
        parent_id       = none,
        display_data    = none,
        requirements    = [[<<"criterion_a">>, <<"criterion_b">>]],
        sends_telemetry = false
    },
    Rec2 = #update_advancements{
        reset               = false,
        advancement_mapping = [{<<"minecraft:story/mine_stone">>, Adv2}],
        identifiers         = [],
        progress_mapping    = []
    },
    RoundTrip(Rec2),

    %% Case 3: Advancement with parent and display data, no background texture (flags=0x02)
    Display3 = #advancement_display{
        title              = TitleTC,
        description        = DescTC,
        icon               = empty,
        frame_type         = 0,
        flags              = 16#02,
        background_texture = undefined,
        x                  = 1.5,
        y                  = -2.0
    },
    Adv3 = #advancement{
        parent_id       = {some, <<"minecraft:story/root">>},
        display_data    = Display3,
        requirements    = [[<<"minecraft:story/mine_stone">>]],
        sends_telemetry = true
    },
    Rec3 = #update_advancements{
        reset               = false,
        advancement_mapping = [{<<"minecraft:story/mine_stone">>, Adv3}],
        identifiers         = [],
        progress_mapping    = []
    },
    RoundTrip(Rec3),

    %% Case 4: Advancement display WITH background texture (flags=0x01)
    Display4 = #advancement_display{
        title              = RootTC,
        description        = WelcomeTC,
        icon               = empty,
        frame_type         = 0,
        flags              = 16#01,
        background_texture = <<"minecraft:textures/gui/advancements/backgrounds/stone.png">>,
        x                  = 0.0,
        y                  = 0.0
    },
    Adv4 = #advancement{
        parent_id       = none,
        display_data    = Display4,
        requirements    = [],
        sends_telemetry = false
    },
    Rec4 = #update_advancements{
        reset               = false,
        advancement_mapping = [{<<"minecraft:story/root">>, Adv4}],
        identifiers         = [],
        progress_mapping    = []
    },
    RoundTrip(Rec4),

    %% Case 5: identifiers to remove
    Rec5 = #update_advancements{
        reset               = false,
        advancement_mapping = [],
        identifiers         = [<<"minecraft:story/old_adv">>, <<"minecraft:end/removed">>],
        progress_mapping    = []
    },
    RoundTrip(Rec5),

    %% Case 6: Progress mapping — one criterion achieved, one not achieved
    Progress6 = #advancement_progress{
        criteria = [
            {<<"minecraft:story/mine_stone">>,  {some, 1700000000000}},
            {<<"minecraft:story/upgrade_tools">>, none}
        ]
    },
    Rec6 = #update_advancements{
        reset               = false,
        advancement_mapping = [],
        identifiers         = [],
        progress_mapping    = [{<<"minecraft:story/mine_stone">>, Progress6}]
    },
    RoundTrip(Rec6),

    %% Case 7: Full packet — advancement + identifiers + progress
    Display7 = #advancement_display{
        title              = ChalTC,
        description        = DragonTC,
        icon               = empty,
        frame_type         = 1,
        flags              = 16#04,
        background_texture = undefined,
        x                  = 5.0,
        y                  = 3.0
    },
    Adv7 = #advancement{
        parent_id       = {some, <<"minecraft:end/root">>},
        display_data    = Display7,
        requirements    = [[<<"minecraft:end/kill_dragon">>]],
        sends_telemetry = true
    },
    Progress7 = #advancement_progress{
        criteria = [{<<"minecraft:end/kill_dragon">>, {some, 1700000000001}}]
    },
    Rec7 = #update_advancements{
        reset               = false,
        advancement_mapping = [{<<"minecraft:end/kill_dragon">>, Adv7}],
        identifiers         = [<<"minecraft:end/removed_adv">>],
        progress_mapping    = [{<<"minecraft:end/kill_dragon">>, Progress7}]
    },
    RoundTrip(Rec7).
