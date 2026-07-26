-module(encode_then_decode).
-include_lib("eunit/include/eunit.hrl").
-include("src/data_types/records.hrl").


bool_test() ->
    {<<>>, true} = decode:decode_type(encode:encode_type(true, bool), bool),
    {<<>>, false} = decode:decode_type(encode:encode_type(false, bool), bool).

byte_test() ->
    {<<>>, 42} = decode:decode_type(encode:encode_type(42, byte), byte),
    {<<>>, -42} = decode:decode_type(encode:encode_type(-42, byte), byte).

ubyte_test() ->
    {<<>>, 200} = decode:decode_type(encode:encode_type(200, ubyte), ubyte).

short_test() ->
    {<<>>, 12345} = decode:decode_type(encode:encode_type(12345, short), short),
    {<<>>, -12345} = decode:decode_type(encode:encode_type(-12345, short), short).

ushort_test() ->
    {<<>>, 50000} = decode:decode_type(encode:encode_type(50000, ushort), ushort).

int_test() ->
    {<<>>, 1000000} = decode:decode_type(encode:encode_type(1000000, int), int),
    {<<>>, -1000000} = decode:decode_type(encode:encode_type(-1000000, int), int).

long_test() ->
    Val = 9223372036854775807,
    {<<>>, Val} = decode:decode_type(encode:encode_type(Val, long), long).

float_test() ->
    Val = 3.14159,
    Encoded = encode:encode_type(Val, float),
    {<<>>, Decoded} = decode:decode_type(Encoded, float),
    ?assert(abs(Val - Decoded) < 0.0001).

double_test() ->
    Val = 3.141592653589793,
    Encoded = encode:encode_type(Val, double),
    {<<>>, Decoded} = decode:decode_type(Encoded, double),
    ?assertEqual(Val, Decoded).

string_test() ->
    Str = "Hello Minecraft!",
    Encoded = encode:encode_type(Str, string),
    {<<>>, Decoded} = decode:decode_type(Encoded, string),
    ?assertEqual(Str, Decoded),
    EncodedBin = encode:encode_type(<<"Hello Minecraft!">>, string),
    {<<>>, Decoded2} = decode:decode_type(EncodedBin, string),
    ?assertEqual(Str, Decoded2).

json_text_component_test() ->
    Map = #{<<"text">> => <<"Hello world">>, <<"color">> => <<"blue">>},
    EncodedMap = encode:encode_type(Map, json_text_component),
    {<<>>, DecodedMap} = decode:decode_type(EncodedMap, json_text_component),
    ?assertEqual(Map, DecodedMap),

    RawJson = <<"{\"text\":\"Hello\",\"bold\":true}">>,
    EncodedRaw = encode:encode_type(RawJson, json_text_component),
    {<<>>, DecodedRaw} = decode:decode_type(EncodedRaw, json_text_component),
    ?assertEqual(#{<<"text">> => <<"Hello">>, <<"bold">> => true}, DecodedRaw),

    CompList = [#{<<"text">> => <<"Part 1">>}, #{<<"text">> => <<"Part 2">>}],
    EncodedList = encode:encode_type(CompList, json_text_component),
    {<<>>, DecodedList} = decode:decode_type(EncodedList, json_text_component),
    ?assertEqual(CompList, DecodedList).

varint_test() ->
    {<<>>, 0} = decode:decode_type(encode:encode_type(0, varint), varint),
    {<<>>, 128} = decode:decode_type(encode:encode_type(128, varint), varint),
    {<<>>, 300} = decode:decode_type(encode:encode_type(300, varint), varint),
    {<<>>, 2147483647} = decode:decode_type(encode:encode_type(2147483647, varint), varint),
    {<<>>, -1} = decode:decode_type(encode:encode_type(-1, varint), varint),
    {<<>>, -2147483648} = decode:decode_type(encode:encode_type(-2147483648, varint), varint).


varlong_test() ->
    {0, <<>>} = decode:decode_type(encode:encode_type(0, varlong), varlong),
    {128, <<>>} = decode:decode_type(encode:encode_type(128, varlong), varlong),
    {300, <<>>} = decode:decode_type(encode:encode_type(300, varlong), varlong),
    {2147483647, <<>>} = decode:decode_type(encode:encode_type(2147483647, varlong), varlong),
    {9223372036854775807, <<>>} = decode:decode_type(encode:encode_type(9223372036854775807, varlong), varlong),
    {-1, <<>>} = decode:decode_type(encode:encode_type(-1, varlong), varlong),
    {-9223372036854775808, <<>>} = decode:decode_type(encode:encode_type(-9223372036854775808, varlong), varlong).

identifier_test() ->
    Str = "minecraft:diamond",
    Encoded = encode:encode_type(Str, identifier),
    {<<>>, Decoded} = decode:decode_type(Encoded, identifier),
    ?assertEqual(Str, Decoded).

position_test() ->
    Pos = {100, -200, 50},
    Encoded = encode:encode_type(Pos, position),
    {<<>>, Decoded} = decode:decode_type(Encoded, position),
    ?assertEqual(Pos, Decoded).

angle_test() ->
    Angle = 45,
    Encoded = encode:encode_type(Angle, angle),
    {<<>>, Decoded} = decode:decode_type(Encoded, angle),
    ?assertEqual(Angle, Decoded).

uuid_test() ->
    UUID = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    Encoded = encode:encode_type(UUID, uuid),
    {<<>>, Decoded} = decode:decode_type(Encoded, uuid),
    ?assertEqual(UUID, Decoded).

bitset_test() ->
    Val1 = -1,
    Encoded1 = encode:encode_type(Val1, bitset),
    {<<>>, Decoded1} = decode:decode_type(Encoded1, bitset),
    ?assertEqual(Val1, Decoded1),
    Val2 = 256,
    Encoded2 = encode:encode_type(Val2, bitset),
    {<<>>, Decoded2} = decode:decode_type(Encoded2, bitset),
    ?assertEqual(Val2, Decoded2).

fixed_bitset_test() ->
    Val1 = -1,
    Encoded1 = encode:encode_type(Val1, fixed_bitset),
    {<<>>, Decoded1} = decode:decode_type(Encoded1, fixed_bitset),
    ?assertEqual(Val1, Decoded1),
    Val2 = 256,
    Encoded2 = encode:encode_type(Val2, fixed_bitset),
    {<<>>, Decoded2} = decode:decode_type(Encoded2, fixed_bitset),
    ?assertEqual(Val2, Decoded2).

optional_test() ->
    EncPresent = encode:encode_type(42, {optional, int, true}),
    ?assertEqual({<<>>, {some, 42}}, decode:decode_type(EncPresent, {optional, int, true})),

    EncAbsent = encode:encode_type(42, {optional, int, false}),
    ?assertEqual({<<>>, none}, decode:decode_type(EncAbsent, {optional, int, false})),

    EncTaggedSome = encode:encode_type({some, "hello"}, {optional, string, true}),
    ?assertEqual({<<>>, {some, "hello"}}, decode:decode_type(EncTaggedSome, {optional, string, true})),

    EncTaggedNone = encode:encode_type(none, {optional, string, false}),
    ?assertEqual({<<>>, none}, decode:decode_type(EncTaggedNone, {optional, string, false})).

prefixed_optional_test() ->
    EncPresent = encode:encode_type({some, 42}, {optional, int}),
    ?assertEqual({<<>>, {some, 42}}, decode:decode_type(EncPresent, {optional, int})),

    EncAbsent = encode:encode_type(none, {optional, int}),
    ?assertEqual({<<>>, none}, decode:decode_type(EncAbsent, {optional, int})),

    EncVal = encode:encode_type("hello", {optional, string}),
    ?assertEqual({<<>>, {some, "hello"}}, decode:decode_type(EncVal, {optional, string})).

id_or_x_test() ->
    IdRef = {id, 5},
    Enc1 = encode:encode_type(IdRef, {id_or_x, string}),
    ?assertEqual({<<>>, IdRef}, decode:decode_type(Enc1, {id_or_x, string})),

    InlineVal = {val, "hello"},
    Enc2 = encode:encode_type(InlineVal, {id_or_x, string}),
    ?assertEqual({<<>>, InlineVal}, decode:decode_type(Enc2, {id_or_x, string})),

    RawVal = "hello",
    Enc3 = encode:encode_type(RawVal, {id_or_x, string}),
    ?assertEqual({<<>>, {val, RawVal}}, decode:decode_type(Enc3, {id_or_x, string})),

    IntVal = {val, 12345},
    Enc4 = encode:encode_type(IntVal, {id_or_x, int}),
    ?assertEqual({<<>>, IntVal}, decode:decode_type(Enc4, {id_or_x, int})).

id_set_test() ->
    TagVal = "minecraft:planks",
    EncTag = encode:encode_type(TagVal, id_set),
    ?assertEqual({<<>>, TagVal}, decode:decode_type(EncTag, id_set)),

    IDsVal = [10, 20, 30, 40],
    EncIDs = encode:encode_type(IDsVal, id_set),
    ?assertEqual({<<>>, IDsVal}, decode:decode_type(EncIDs, id_set)),

    EmptyIDs = [],
    EncEmpty = encode:encode_type(EmptyIDs, id_set),
    ?assertEqual({<<>>, EmptyIDs}, decode:decode_type(EncEmpty, id_set)).

array_test() ->
    ListInt = [10, 20, 30, 40],
    EncodedInt = encode:encode_type(ListInt, {array, int}),
    ?assertEqual({<<>>, ListInt}, decode:decode_type(EncodedInt, {array, 4, int})),

    ListStr = ["foo", "bar", "baz"],
    EncodedStr = encode:encode_type(ListStr, {array, string}),
    ?assertEqual({<<>>, ListStr}, decode:decode_type(EncodedStr, {array, 3, string})),

    EmptyList = [],
    EncodedEmpty = encode:encode_type(EmptyList, {array, int}),
    ?assertEqual({<<>>, []}, decode:decode_type(EncodedEmpty, {array, 0, int})).

prefixed_array_test() ->
    ListInt = [10, 20, 30, 40],
    EncodedInt = encode:encode_type(ListInt, {prefixed_array, int}),
    ?assertEqual({<<>>, ListInt}, decode:decode_type(EncodedInt, {prefixed_array, int})),

    ListStr = ["foo", "bar", "baz"],
    EncodedStr = encode:encode_type(ListStr, {prefixed_array, string}),
    ?assertEqual({<<>>, ListStr}, decode:decode_type(EncodedStr, {prefixed_array, string})),

    EmptyList = [],
    EncodedEmpty = encode:encode_type(EmptyList, {prefixed_array, int}),
    ?assertEqual({<<>>, []}, decode:decode_type(EncodedEmpty, {prefixed_array, int})),

    ShortPrefixedList = [100, 200],
    EncodedShortPrefixed = encode:encode_type(ShortPrefixedList, {prefixed_array, short, int}),
    ?assertEqual({<<>>, ShortPrefixedList}, decode:decode_type(EncodedShortPrefixed, {prefixed_array, short, int})).

enum_test() ->
    Val = 42,
    ?assertEqual({<<>>, Val}, decode:decode_type(encode:encode_type(Val, enum), enum)),
    ?assertEqual({<<>>, Val}, decode:decode_type(encode:encode_type(Val, {enum, varint}), {enum, varint})),
    ?assertEqual({<<>>, Val}, decode:decode_type(encode:encode_type(Val, {enum, byte}), {enum, byte})),
    ?assertEqual({<<>>, Val}, decode:decode_type(encode:encode_type(Val, {enum, int}), {enum, int})),

    EnumList = [north, south, east, west],
    ?assertEqual({<<>>, south}, decode:decode_type(encode:encode_type(south, {enum, EnumList}), {enum, EnumList})),
    ?assertEqual({<<>>, west}, decode:decode_type(encode:encode_type(west, {enum, byte, EnumList}), {enum, byte, EnumList})).

byte_array_test() ->
    Bytes = <<"some random binary data">>,
    ?assertEqual({<<>>, Bytes}, decode:decode_type(encode:encode_type(Bytes, byte_array), byte_array)),
    ?assertEqual({<<>>, Bytes}, decode:decode_type(encode:encode_type(Bytes, {byte_array, byte_size(Bytes)}), {byte_array, byte_size(Bytes)})),
    ?assertEqual({<<>>, Bytes}, decode:decode_type(encode:encode_type(Bytes, {byte_array, varint}), {byte_array, varint})),
    ?assertEqual({<<>>, Bytes}, decode:decode_type(encode:encode_type(Bytes, {byte_array, short}), {byte_array, short})),
    Str = "string data",
    ?assertEqual({<<>>, list_to_binary(Str)}, decode:decode_type(encode:encode_type(Str, {byte_array, varint}), {byte_array, varint})).

slot_test() ->
    %% Empty slot
    {<<>>, empty} = decode:decode_type(encode:encode_type(empty, slot), slot),
    %% Non-empty, no components
    Slot1 = {3, 42, [], []},
    {<<>>, Slot1} = decode:decode_type(encode:encode_type(Slot1, slot), slot),
    %% Multiple add and remove components
    Slot2 = {1, 7, [{3, <<1, 2, 3>>}, {10, <<255>>}], [5, 9]},
    {<<>>, Slot2} = decode:decode_type(encode:encode_type(Slot2, slot), slot).

hashed_slot_test() ->
    %% Empty hashed slot
    {<<>>, empty} = decode:decode_type(encode:encode_type(empty, hashed_slot), hashed_slot),
    %% Non-empty, no components
    HSlot1 = {5, 1, [], []},
    {<<>>, HSlot1} = decode:decode_type(encode:encode_type(HSlot1, hashed_slot), hashed_slot),
    %% Multiple add and remove components; Hash values include a negative (testing sign)
    HSlot2 = {10, 3, [{2, 99999}, {7, -1}], [4, 8]},
    {<<>>, HSlot2} = decode:decode_type(encode:encode_type(HSlot2, hashed_slot), hashed_slot).

sound_event_test() ->
    %% Fixed range present (true)
    Sound1 = {"minecraft:entity.experience_orb.pickup", true, 16.0},
    Encoded1 = encode:encode_type(Sound1, sound_event),
    {<<>>, Decoded1} = decode:decode_type(Encoded1, sound_event),
    ?assertEqual({"minecraft:entity.experience_orb.pickup", true, 16.0}, Decoded1),

    %% Fixed range absent (false with undefined)
    Sound2 = {"minecraft:entity.pig.ambient", false, undefined},
    Encoded2 = encode:encode_type(Sound2, sound_event),
    {<<>>, Decoded2} = decode:decode_type(Encoded2, sound_event),
    ?assertEqual({"minecraft:entity.pig.ambient", false, undefined}, Decoded2),

    %% 2-tuple input format {SoundName, false}
    Sound3 = {"minecraft:ui.button.click", false},
    Encoded3 = encode:encode_type(Sound3, sound_event),
    {<<>>, Decoded3} = decode:decode_type(Encoded3, sound_event),
    ?assertEqual({"minecraft:ui.button.click", false, undefined}, Decoded3).

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
    ?assertEqual(MapInput, DecodedMap),

    %% Test list input
    ListInput = [relative_x, relative_pitch, rotate_velocity],
    EncodedList = encode:encode_type(ListInput, teleport_flags),
    {<<>>, DecodedList} = decode:decode_type(EncodedList, teleport_flags),
    ?assertEqual(true, maps:get(relative_x, DecodedList)),
    ?assertEqual(false, maps:get(relative_y, DecodedList)),
    ?assertEqual(true, maps:get(relative_pitch, DecodedList)),
    ?assertEqual(true, maps:get(rotate_velocity, DecodedList)),

    %% Test integer bitfield input
    IntInput = 16#0105,
    EncodedInt = encode:encode_type(IntInput, teleport_flags),
    {<<>>, DecodedInt} = decode:decode_type(EncodedInt, teleport_flags),
    ?assertEqual(true, maps:get(relative_x, DecodedInt)),
    ?assertEqual(true, maps:get(relative_z, DecodedInt)),
    ?assertEqual(true, maps:get(rotate_velocity, DecodedInt)).

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
    ?assertEqual({<<>>, ItemStack}, decode:decode_type(encode:encode_type(ItemStack, slot_display), slot_display)),

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
    ?assertEqual({<<>>, 123}, decode:decode_type(EncodedX, TypeSpec)),
    ?assertEqual({<<>>, "test_string"}, decode:decode_type(EncodedY, TypeSpec)).

game_profile_test() ->
    UUID = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    Username = "Steve",
    Properties = [
        {"textures", "base64_texture_data", {some, "sig_abc"}},
        {"cape", "cape_data", none}
    ],
    Profile = {UUID, Username, Properties},
    Encoded = encode:encode_type(Profile, game_profile),
    ?assertEqual({<<>>, Profile}, decode:decode_type(Encoded, game_profile)).

resolvable_profile_roundtrip_test() ->
    UUID = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    %% Test Partial (ProfileKind = 0)
    PartialData = {0, {{some, "Steve"}, {some, UUID}, [{"textures", "base64_val", {some, "sig123"}}]}, {some, "minecraft:textures/skin1"}, none, none, {some, 0}},
    TypeSpecPartial = {resolvable_profile, true, false, false, true},
    EncodedPartial = encode:encode_type(PartialData, TypeSpecPartial),
    ?assertEqual({<<>>, PartialData}, decode:decode_type(EncodedPartial, TypeSpecPartial)),

    %% Test Complete (ProfileKind = 1)
    GameProfile = {UUID, "Alex", [{"textures", "val2", none}]},
    CompleteData = {1, GameProfile, none, {some, "minecraft:textures/cape1"}, none, none},
    TypeSpecComplete = {resolvable_profile, false, true, false, false},
    EncodedComplete = encode:encode_type(CompleteData, TypeSpecComplete),
    ?assertEqual({<<>>, CompleteData}, decode:decode_type(EncodedComplete, TypeSpecComplete)).

debug_subscription_event_test() ->
    Records = [
        #dedicated_server_tick_time{type = dedicated_server_tick_time},
        #bee{type = bee, hive_position = {some, {1,2,3}}, flower_position = none, travel_ticks = 100, blacklisted_hives = [{4,5,6}]},
        #villager_brain{type = villager_brain, name = "villager", profession = "farmer", xp = 10, health = 20.0, max_health = 20.0, inventory = "", wants_golem = false, anger_level = 0, activities = ["work"], behaviors = [], memories = [], gossips = [], pois = [{1,2,3}], potential_pois = []},
        #breeze{type = breeze, attack_target = {some, 5}, jump_target = none},
        #goal_selector{type = goal_selector, priority = 1, is_running = true, name = "swim"},
        #entity_path{type = entity_path, reached = true, next_block_index = 10, block_position = {10,20,30}, nodes = [{1,2,3, 1.0, 0.5, true, 0, 1}], target_nodes = [], open_set = [], closed_set = [], max_node_distance = 2.5},
        #entity_block_intersection{type = entity_block_intersection, id = 2},
        #bee_hive{type = bee_hive, hive_type = 5, occupant_count = 3, honey_level = 4, sedated = true},
        #poi{type = poi, position = {10,20,30}, poi_type = 1, free_ticket_count = 2},
        #redstone_wire_orientation{type = redstone_wire_orientation, id = 42},
        #village_section{type = village_section},
        #raid{type = raid, positions = [{1,1,1}, {2,2,2}]},
        #structure{type = structure, structures = [{{0,0,0,20,20,20}, [{{0,0,0,10,10,10}, true}]}]},
        #game_event_listener{type = game_event_listener, listener_radius = 8},
        #neighbor_update{type = neighbor_update, position = {7,8,9}},
        #game_event{type = game_event, event = 3, x = 1.5, y = 2.5, z = 3.5}
    ],
    lists:foreach(fun(Rec) ->
        Enc = encode:encode_type(Rec, debug_subscription_event),
        ?assertEqual({<<>>, Rec}, decode:decode_type(Enc, debug_subscription_event))
    end, Records).

debug_subscription_update_test() ->
    Bee = #bee{type = bee, hive_position = {some, {1,2,3}}, flower_position = none, travel_ticks = 100, blacklisted_hives = [{4,5,6}]},
    %% Present update (using {some, Record})
    EncPresent = encode:encode_type({some, Bee}, debug_subscription_update),
    ?assertEqual({<<>>, {some, Bee}}, decode:decode_type(EncPresent, debug_subscription_update)),

    %% Present update (using {Type, {some, Record}})
    EncPresent2 = encode:encode_type({1, {some, Bee}}, debug_subscription_update),
    ?assertEqual({<<>>, {some, Bee}}, decode:decode_type(EncPresent2, debug_subscription_update)),

    %% Absent update (using {Type, none})
    EncAbsent = encode:encode_type({1, none}, debug_subscription_update),
    ?assertEqual({<<>>, none}, decode:decode_type(EncAbsent, debug_subscription_update)).




