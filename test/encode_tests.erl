-module(encode_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/data_types/records.hrl").
-include("src/data_types/components/component_records.hrl").


bool_test() ->
    ?assertEqual(<<1>>, encode:encode_type(true, bool)),
    ?assertEqual(<<0>>, encode:encode_type(false, bool)).

byte_test() ->
    ?assertEqual(<<127>>, encode:encode_type(127, byte)),
    ?assertEqual(<<-128>>, encode:encode_type(-128, byte)).

ubyte_test() ->
    ?assertEqual(<<255>>, encode:encode_type(255, ubyte)),
    ?assertEqual(<<0>>, encode:encode_type(0, ubyte)).

short_test() ->
    ?assertEqual(<<32767:16/signed-integer>>, encode:encode_type(32767, short)),
    ?assertEqual(<<-32768:16/signed-integer>>, encode:encode_type(-32768, short)).

ushort_test() ->
    ?assertEqual(<<65535:16/unsigned-integer>>, encode:encode_type(65535, ushort)).

int_test() ->
    ?assertEqual(<<2147483647:32/signed-integer>>, encode:encode_type(2147483647, int)),
    ?assertEqual(<<-2147483648:32/signed-integer>>, encode:encode_type(-2147483648, int)).

long_test() ->
    Val = 9223372036854775807,
    ?assertEqual(<<Val:64/signed-integer>>, encode:encode_type(Val, long)).

float_test() ->
    Val = 3.14159,
    ?assertEqual(<<Val:32/float>>, encode:encode_type(Val, float)).

double_test() ->
    Val = 3.141592653589793,
    ?assertEqual(<<Val:64/float>>, encode:encode_type(Val, double)).

string_test() ->
    Str = "Hello Minecraft!",
    ?assertEqual(<<16, "Hello Minecraft!">>, encode:encode_type(Str, string)),
    ?assertEqual(<<16, "Hello Minecraft!">>, encode:encode_type(<<"Hello Minecraft!">>, string)).

varint_test() ->
    ?assertEqual(<<0>>, encode:encode_type(0, varint)),
    ?assertEqual(<<1>>, encode:encode_type(1, varint)),
    ?assertEqual(<<127>>, encode:encode_type(127, varint)),
    ?assertEqual(<<128, 1>>, encode:encode_type(128, varint)),
    ?assertEqual(<<255, 1>>, encode:encode_type(255, varint)),
    ?assertEqual(<<255, 255, 255, 255, 7>>, encode:encode_type(2147483647, varint)),
    ?assertEqual(<<255, 255, 255, 255, 15>>, encode:encode_type(-1, varint)),
    ?assertEqual(<<128, 128, 128, 128, 8>>, encode:encode_type(-2147483648, varint)).

varlong_test() ->
    ?assertEqual(<<0>>, encode:encode_type(0, varlong)),
    ?assertEqual(<<1>>, encode:encode_type(1, varlong)),
    ?assertEqual(<<127>>, encode:encode_type(127, varlong)),
    ?assertEqual(<<128, 1>>, encode:encode_type(128, varlong)),
    ?assertEqual(<<255, 1>>, encode:encode_type(255, varlong)),
    ?assertEqual(<<255, 255, 255, 255, 7>>, encode:encode_type(2147483647, varlong)),
    ?assertEqual(<<255, 255, 255, 255, 255, 255, 255, 255, 127>>, encode:encode_type(9223372036854775807, varlong)),
    ?assertEqual(<<255, 255, 255, 255, 255, 255, 255, 255, 255, 1>>, encode:encode_type(-1, varlong)),
    ?assertEqual(<<128, 128, 128, 128, 128, 128, 128, 128, 128, 1>>, encode:encode_type(-9223372036854775808, varlong)).

identifier_test() ->
    Str = "minecraft:diamond",
    ?assertEqual(<<17, "minecraft:diamond">>, encode:encode_type(Str, identifier)).

position_test() ->
    X = 100,
    Z = -200,
    Y = 50,
    Expected = <<X:26/signed-integer, Z:26/signed-integer, Y:12/signed-integer>>,
    ?assertEqual(Expected, encode:encode_type({X, Z, Y}, position)).

angle_test() ->
    ?assertEqual(<<45>>, encode:encode_type(45, angle)),
    ?assertEqual(<<128>>, encode:encode_type(-128, angle)).

uuid_test() ->
    UUIDBin = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    ?assertEqual(UUIDBin, encode:encode_type(UUIDBin, uuid)).

bitset_test() ->
    ?assertEqual(<<1, 16#FF>>, encode:encode_type(-1, bitset)),
    ?assertEqual(<<2, 1, 0>>, encode:encode_type(256, bitset)).

fixed_bitset_test() ->
    ?assertEqual(<<8, 16#FF>>, encode:encode_type(-1, fixed_bitset)),
    ?assertEqual(<<16, 1, 0>>, encode:encode_type(256, fixed_bitset)).

optional_test() ->
    ?assertEqual(<<5, "hello">>, encode:encode_type({some, "hello"}, {optional, string, true})),
    ?assertEqual(<<>>, encode:encode_type(none, {optional, string, false})).

prefixed_optional_test() ->
    ?assertEqual(<<1, 42:32/signed-integer>>, encode:encode_type({some, 42}, {optional, int})),
    ?assertEqual(<<0>>, encode:encode_type(none, {optional, int})),
    ?assertEqual(<<0>>, encode:encode_type(undefined, {optional, int})).

id_or_x_test() ->
    ?assertEqual(<<1>>, encode:encode_type({id, 0}, id_or_x)),
    ?assertEqual(<<6>>, encode:encode_type({id, 5}, {id_or_x, string})),
    ?assertEqual(<<128, 1>>, encode:encode_type({id, 127}, {id_or_x, int})),
    ?assertEqual(<<0, 5, "hello">>, encode:encode_type({val, "hello"}, {id_or_x, string})),
    ?assertEqual(<<0, 5, "hello">>, encode:encode_type({value, "hello"}, {id_or_x, string})),
    ?assertEqual(<<0, 5, "hello">>, encode:encode_type({inline, "hello"}, {id_or_x, string})),
    ?assertEqual(<<0, 5, "hello">>, encode:encode_type("hello", {id_or_x, string})),
    ?assertEqual(<<0, 42:32/signed-integer>>, encode:encode_type(42, {id_or_x, int})).

id_set_test() ->
    ?assertEqual(<<0, 14, "minecraft:wool">>, encode:encode_type(<<"minecraft:wool">>, id_set)),
    ?assertEqual(<<0, 14, "minecraft:wool">>, encode:encode_type({tag, <<"minecraft:wool">>}, id_set)),
    ?assertEqual(<<4, 1, 2, 3>>, encode:encode_type([1, 2, 3], id_set)),
    ?assertEqual(<<4, 1, 2, 3>>, encode:encode_type({ids, [1, 2, 3]}, id_set)),
    ?assertEqual(<<1>>, encode:encode_type([], id_set)).


array_test() ->
    ?assertEqual(<<1, 0>>, encode:encode_type([true, false], {array, bool})),
    ?assertEqual(<<100:32/signed-integer>>, encode:encode_type([100, 200], {array, 1, int})).

prefixed_array_test() ->
    ?assertEqual(<<2, 10, 20>>, encode:encode_type([10, 20], {prefixed_array, byte})),
    ?assertEqual(<<0>>, encode:encode_type([], {prefixed_array, byte})),
    ?assertEqual(<<2, 100:32/signed-integer, 200:32/signed-integer>>, encode:encode_type([100, 200], {prefixed_array, int})),
    ?assertEqual(<<0, 2, 10, 20>>, encode:encode_type([10, 20], {prefixed_array, short, byte})).

enum_test() ->
    ?assertEqual(<<42>>, encode:encode_type(42, enum)),
    ?assertEqual(<<42>>, encode:encode_type(42, {enum, varint})),
    ?assertEqual(<<42>>, encode:encode_type(42, {enum, byte})),
    ?assertEqual(<<42:32/signed-integer>>, encode:encode_type(42, {enum, int})),
    ?assertEqual(<<1>>, encode:encode_type(south, {enum, [north, south, east, west]})),
    ?assertEqual(<<2>>, encode:encode_type(east, {enum, byte, [north, south, east, west]})).

byte_array_test() ->
    ?assertEqual(<<"hello">>, encode:encode_type(<<"hello">>, byte_array)),
    ?assertEqual(<<"hello">>, encode:encode_type("hello", byte_array)),
    ?assertEqual(<<"hello">>, encode:encode_type(<<"helloworld">>, {byte_array, 5})),
    ?assertEqual(<<5, "hello">>, encode:encode_type(<<"hello">>, {byte_array, varint})),
    ?assertEqual(<<0, 5, "hello">>, encode:encode_type("hello", {byte_array, short})).

slot_test() ->
    %% Empty slot: just VarInt(0)
    ?assertEqual(<<0>>, encode:encode_type(empty, slot)),
    ?assertEqual(<<0>>, encode:encode_type(0, slot)),
    ?assertEqual(<<0>>, encode:encode_type(#slot{item_count = 0}, slot)),
    %% Non-empty, no components: ItemCount=1, ItemID=5, NAdd=0, NRemove=0
    ?assertEqual(<<1, 5, 0, 0>>, encode:encode_type({1, 5, [], []}, slot)),
    ?assertEqual(<<1, 5, 0, 0>>, encode:encode_type(#slot{item_count = 1, itemID = 5, components_to_add = [], components_to_remove = []}, slot)),
    %% With add component using record: TypeId=3 (damage=2), remove TypeId=7
    ?assertEqual(<<2, 10, 1, 1, 3, 2, 7>>,
                 encode:encode_type({2, 10, [{3, #damage{type = 'minecraft:damage', damage = 2}}], [7]}, slot)),
    ?assertEqual(<<2, 10, 1, 1, 3, 2, 7>>,
                 encode:encode_type(#slot{item_count = 2, itemID = 10,
                                         components_to_add = [{3, #damage{type = 'minecraft:damage', damage = 2}}],
                                         components_to_remove = [7]}, slot)),
    %% With multiple add components (max_stack_size=64 and unbreakable) and multiple remove components
    ?assertEqual(<<5, 20, 2, 2, 1, 64, 4, 2, 5>>,
                 encode:encode_type(#slot{item_count = 5, itemID = 20,
                                         components_to_add = [
                                             {1, #max_stack_size{type = 'minecraft:max_stack_size', max_stack_size = 64}},
                                             {4, #unbreakable{type = 'minecraft:unbreakable'}}
                                         ],
                                         components_to_remove = [2, 5]}, slot)).

hashed_slot_test() ->
    %% Empty hashed slot: Boolean false
    ?assertEqual(<<0>>, encode:encode_type(empty, hashed_slot)),
    %% Non-empty, no components: HasItem=true, ItemID=5, ItemCount=1, NAdd=0, NRemove=0
    ?assertEqual(<<1, 5, 1, 0, 0>>, encode:encode_type({5, 1, [], []}, hashed_slot)),
    %% With one add {TypeId=3, Hash=12345} and one remove TypeId=7:
    %% HasItem=true, ItemID=5, ItemCount=1, NAdd=1,
    %% add: TypeId=3, Hash=12345 as Int32, NRemove=1, remove: TypeId=7
    ?assertEqual(<<1, 5, 1, 1, 3, 12345:32/signed-integer, 1, 7>>,
                 encode:encode_type({5, 1, [{3, 12345}], [7]}, hashed_slot)).

text_component_test() ->
    Comp = #{type => <<"text">>, text => <<"Hello">>},
    EncBin = encode:encode_type(Comp, text_component),
    ?assert(is_binary(EncBin)),
    {Rest, #text_component{component_map = DecodedComp}} = decode:decode_type(EncBin, text_component),
    ?assertEqual(<<>>, Rest),
    ?assertEqual(<<"text">>, maps:get(type, DecodedComp)),
    ?assertEqual(<<"Hello">>, maps:get(text, DecodedComp)).

teleport_flags_test() ->
    %% Int input
    ?assertEqual(<<0:32/signed-integer>>, encode:encode_type(0, teleport_flags)),
    ?assertEqual(<<511:32/signed-integer>>, encode:encode_type(16#01FF, teleport_flags)),

    %% Map input
    MapVal = #{
        relative_x => true,
        relative_z => true,
        rotate_velocity => true
    },
    ExpectedInt = 16#0001 bor 16#0004 bor 16#0100,
    ?assertEqual(<<ExpectedInt:32/signed-integer>>, encode:encode_type(MapVal, teleport_flags)),

    %% List input
    ListVal = [relative_x, relative_z, rotate_velocity],
    ?assertEqual(<<ExpectedInt:32/signed-integer>>, encode:encode_type(ListVal, teleport_flags)),

    %% Empty map / list
    ?assertEqual(<<0:32/signed-integer>>, encode:encode_type(#{}, teleport_flags)),
    ?assertEqual(<<0:32/signed-integer>>, encode:encode_type([], teleport_flags)).

slot_display_test() ->
    ?assertEqual(<<0>>, encode:encode_type(#empty{type = 'minecraft:empty'}, slot_display)),
    ?assertEqual(<<1>>, encode:encode_type(#any_fuel{type = 'minecraft:any_fuel'}, slot_display)),
    ?assertEqual(<<4, 42>>, encode:encode_type(#item{type = 'minecraft:item', item_type = 42}, slot_display)),
    ?assertEqual(<<6, 15, "minecraft:stone">>, encode:encode_type(#tag{type = 'minecraft:tag', tag = "minecraft:stone"}, slot_display)).

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
    ?assertEqual(<<0, 1, 4, 10, 0, 0>>, encode:encode_type(Shapeless, recipe_display)),

    Shaped = #crafting_shaped{
        type = 'minecraft:crafting_shaped',
        width = 1,
        height = 1,
        ingredients_count = 1,
        ingredients = [ItemSlot],
        result = EmptySlot,
        crafting_station = EmptySlot
    },
    ?assertEqual(<<1, 1, 1, 1, 4, 10, 0, 0>>, encode:encode_type(Shaped, recipe_display)),

    Furnace = #furnace{
        type = 'minecraft:furnace',
        ingredient = ItemSlot,
        fuel = EmptySlot,
        result = EmptySlot,
        crafting_station = EmptySlot,
        cooking_time = 200,
        experience = 0.35
    },
    ExpBin = <<0.35:32/float>>,
    ?assertEqual(<<2, 4, 10, 0, 0, 0, 200, 1, ExpBin/binary>>, encode:encode_type(Furnace, recipe_display)),

    Stonecutter = #stonecutter{
        type = 'minecraft:stonecutter',
        ingredient = ItemSlot,
        result = EmptySlot,
        crafting_station = EmptySlot
    },
    ?assertEqual(<<3, 4, 10, 0, 0>>, encode:encode_type(Stonecutter, recipe_display)),

    Smithing = #smithing{
        type = 'minecraft:smithing',
        template = ItemSlot,
        base = EmptySlot,
        addition = EmptySlot,
        result = EmptySlot,
        crafting_station = EmptySlot
    },
    ?assertEqual(<<4, 4, 10, 0, 0, 0, 0>>, encode:encode_type(Smithing, recipe_display)).

either_x_or_y_test() ->
    %% X variant (encoded with boolean true byte 1)
    ?assertEqual(<<1, 42>>, encode:encode_type({left, 42}, {either_x_or_y, byte, string})),
    ?assertEqual(<<1, 42>>, encode:encode_type({x, 42}, {either_x_or_y, byte, string})),
    ?assertEqual(<<1, 42>>, encode:encode_type({true, 42}, {either_x_or_y, byte, string})),
    %% Y variant (encoded with boolean false byte 0)
    ?assertEqual(<<0, 5, "hello">>, encode:encode_type({right, <<"hello">>}, {either_x_or_y, byte, string})),
    ?assertEqual(<<0, 5, "hello">>, encode:encode_type({y, <<"hello">>}, {either_x_or_y, byte, string})),
    ?assertEqual(<<0, 5, "hello">>, encode:encode_type({false, <<"hello">>}, {either_x_or_y, byte, string})).

game_profile_test() ->
    UUIDBin = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    UsernameBin = <<7, "Player1">>,
    CountBin = <<2>>,
    Prop1Bin = <<8, "textures", 4, "val1", 1, 4, "sig1">>,
    Prop2Bin = <<4, "cape", 4, "val2", 0>>,
    ExpectedBin = <<UUIDBin/binary, UsernameBin/binary, CountBin/binary, Prop1Bin/binary, Prop2Bin/binary>>,
    Profile = {UUIDBin, "Player1", [
        {"textures", "val1", {some, "sig1"}},
        {"cape", "val2", none}
    ]},
    ?assertEqual(ExpectedBin, encode:encode_type(Profile, game_profile)).

resolvable_profile_partial_test() ->
    ProfileKindBin = <<0>>,
    UserBin = <<1, 5, "Steve">>,
    UuidBin = <<0>>,
    CountBin = <<1>>,
    Prop1Bin = <<8, "textures", 8, "tex_data", 0>>,
    BodyBin = <<14, "textures/skin1">>,
    ModelBin = <<1>>,
    ExpectedBin = <<ProfileKindBin/binary, UserBin/binary, UuidBin/binary, CountBin/binary, Prop1Bin/binary, BodyBin/binary, ModelBin/binary>>,
    
    Data = {0, {{some, "Steve"}, none, [{"textures", "tex_data", none}]}, {some, "textures/skin1"}, none, none, {some, 1}},
    TypeSpec = {resolvable_profile, true, false, false, true},
    ?assertEqual(ExpectedBin, encode:encode_type(Data, TypeSpec)),
    ?assertEqual(ExpectedBin, encode:encode_type(Data, resolvable_profile)).

resolvable_profile_complete_test() ->
    ProfileKindBin = <<1>>,
    UUID = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    UserBin = <<4, "Alex">>,
    CountBin = <<0>>,
    ExpectedBin = <<ProfileKindBin/binary, UUID/binary, UserBin/binary, CountBin/binary>>,
    
    Data = {1, {UUID, "Alex", []}, none, none, none, none},
    TypeSpec = {resolvable_profile, false, false, false, false},
    ?assertEqual(ExpectedBin, encode:encode_type(Data, TypeSpec)),
    ?assertEqual(ExpectedBin, encode:encode_type(Data, resolvable_profile)).








