-module(encode_tests).
-include("src/data_types/records.hrl").

-include("src/data_types/components/component_records.hrl").
-include_lib("eunit/include/eunit.hrl").
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
    ?assertEqual(<<1, 0, 0, 0, 0, 0, 0, 1, 0>>, encode:encode_type(256, bitset)).

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

lp_vec3_test() ->
    ?assertEqual(<<0>>, encode:encode_type(#lp_vec3{x = 0.0, y = 0.0, z = 0.0}, lp_vec3)),
    ?assertEqual(<<0>>, encode:encode_type({0.0, 0.0, 0.0}, lp_vec3)),
    ?assertEqual(<<0>>, encode:encode_type([0.0, 0.0, 0.0], lp_vec3)),
    
    Sample2Bin = <<241, 255, 0, 0, 255, 255>>,
    ?assertEqual(Sample2Bin, encode:encode_type(#lp_vec3{x = 1.0, y = 0.0, z = -1.0}, lp_vec3)),
    ?assertEqual(Sample2Bin, encode:encode_type({1.0, 0.0, -1.0}, lp_vec3)),

    Sample3Bin = <<246, 255, 64, 1, 5, 31, 2>>,
    ?assertEqual(Sample3Bin, encode:encode_type(#lp_vec3{x = 10.0, y = 0.2, z = -5.0}, lp_vec3)),

    Sample4Bin = <<245, 255, 127, 255, 0, 7, 144, 241, 1>>,
    ?assertEqual(Sample4Bin, encode:encode_type(#lp_vec3{x = 123457.0, y = 15.071, z = 0.0}, lp_vec3)).

chat_type_test() ->
    NbtVal = [{tag_compound, "", []}],
    Rec = #chat_type{
        translation_key = "chat.type.text",
        parameters = [sender, content],
        style = NbtVal
    },
    KeyBin = encode:encode_type("chat.type.text", string),
    ParamsBin = encode:encode_type([sender, content], {prefixed_array, {enum, [sender, target, content]}}),
    StyleBin = encode:encode_type(NbtVal, nbt),
    ExpectedBin = <<KeyBin/binary, ParamsBin/binary, StyleBin/binary>>,
    ?assertEqual(ExpectedBin, encode:encode_type(Rec, chat_type)).

set_equipment_test() ->
    EmptySlot = #slot{item_count = 0, itemID = undefined, components_to_add = [], components_to_remove = []},
    Rec = #set_equipment{
        entity_id = 1,
        equipment = [
            {0, EmptySlot},
            {1, EmptySlot}
        ]
    },
    EntityBin = encode:encode_type(1, varint),
    Slot0Bin = encode:encode_type(16#80, byte),
    Item0Bin = encode:encode_type(EmptySlot, slot),
    Slot1Bin = encode:encode_type(1, byte),
    Item1Bin = encode:encode_type(EmptySlot, slot),
    ExpectedBin = <<EntityBin/binary, Slot0Bin/binary, Item0Bin/binary, Slot1Bin/binary, Item1Bin/binary>>,
    ?assertEqual(ExpectedBin, encode:encode_type(Rec, set_equipment)).

set_objective_test() ->
    %% Test Mode 1 (Remove)
    Rec1 = #set_objective{
        objective_name = "obj1",
        mode = 1
    },
    Name1Bin = encode:encode_type("obj1", string),
    Mode1Bin = encode:encode_type(1, byte),
    ?assertEqual(<<Name1Bin/binary, Mode1Bin/binary>>, encode:encode_type(Rec1, set_objective)),

    %% Test Mode 0 (Create with no format)
    Rec0 = #set_objective{
        objective_name = "obj0",
        mode = 0,
        objective_value = #{type => <<"text">>, text => <<"Hello">>},
        type = integer,
        number_format = undefined
    },
    Name0Bin = encode:encode_type("obj0", string),
    Mode0Bin = encode:encode_type(0, byte),
    ValBin = encode:encode_type(#{type => <<"text">>, text => <<"Hello">>}, text_component),
    TypeBin = encode:encode_type(0, {enum, varint}),
    HasNFBin = encode:encode_type(false, bool),
    ExpectedMode0Bin = <<Name0Bin/binary, Mode0Bin/binary, ValBin/binary, TypeBin/binary, HasNFBin/binary>>,
    ?assertEqual(ExpectedMode0Bin, encode:encode_type(Rec0, set_objective)).

set_player_team_test() ->
    %% Method 1 (Remove team)
    Rec1 = #set_player_team{
        team_name = "red_team",
        method = 1
    },
    Name1Bin = encode:encode_type("red_team", string),
    Meth1Bin = encode:encode_type(1, byte),
    ?assertEqual(<<Name1Bin/binary, Meth1Bin/binary>>, encode:encode_type(Rec1, set_player_team)),

    %% Method 0 (Create team)
    Rec0 = #set_player_team{
        team_name = "blue_team",
        method = 0,
        team_display_name = #{type => <<"text">>, text => <<"Blue Team">>},
        team_prefix = #{type => <<"text">>, text => <<"[Blue] ">>},
        team_suffix = #{type => <<"text">>, text => <<" [Member]">>},
        name_tag_visibility = always,
        collision_rule = push_other_teams,
        team_color = blue,
        friendly_flags = 3,
        entities = ["Player1", "Player2"]
    },
    Name0Bin = encode:encode_type("blue_team", string),
    Meth0Bin = encode:encode_type(0, byte),
    DispBin = encode:encode_type(#{type => <<"text">>, text => <<"Blue Team">>}, text_component),
    PrefBin = encode:encode_type(#{type => <<"text">>, text => <<"[Blue] ">>}, text_component),
    SuffBin = encode:encode_type(#{type => <<"text">>, text => <<" [Member]">>}, text_component),
    VisBin = encode:encode_type(0, {enum, varint}),
    CollBin = encode:encode_type(2, {enum, varint}),
    ColBin = encode:encode_type(9, {enum, varint}),
    FlagsBin = encode:encode_type(3, byte),
    EntBin = encode:encode_type(["Player1", "Player2"], {prefixed_array, string}),
    Expected0Bin = <<Name0Bin/binary, Meth0Bin/binary, DispBin/binary, PrefBin/binary, SuffBin/binary, VisBin/binary, CollBin/binary, ColBin/binary, FlagsBin/binary, EntBin/binary>>,
    ?assertEqual(Expected0Bin, encode:encode_type(Rec0, set_player_team)),

    %% Method 2 (Update team info)
    Rec2 = #set_player_team{
        team_name = "blue_team",
        method = 2,
        team_display_name = #{type => <<"text">>, text => <<"Blue Team Info">>},
        team_prefix = #{type => <<"text">>, text => <<"">>},
        team_suffix = #{type => <<"text">>, text => <<"">>},
        name_tag_visibility = never,
        collision_rule = never,
        team_color = red,
        friendly_flags = 1
    },
    Meth2Bin = encode:encode_type(2, byte),
    Disp2Bin = encode:encode_type(#{type => <<"text">>, text => <<"Blue Team Info">>}, text_component),
    Pref2Bin = encode:encode_type(#{type => <<"text">>, text => <<"">>}, text_component),
    Suff2Bin = encode:encode_type(#{type => <<"text">>, text => <<"">>}, text_component),
    Vis2Bin = encode:encode_type(1, {enum, varint}),
    Coll2Bin = encode:encode_type(1, {enum, varint}),
    Col2Bin = encode:encode_type(12, {enum, varint}),
    Flags2Bin = encode:encode_type(1, byte),
    Expected2Bin = <<Name0Bin/binary, Meth2Bin/binary, Disp2Bin/binary, Pref2Bin/binary, Suff2Bin/binary, Vis2Bin/binary, Coll2Bin/binary, Col2Bin/binary, Flags2Bin/binary>>,
    ?assertEqual(Expected2Bin, encode:encode_type(Rec2, set_player_team)),

    %% Method 3 (Add entities)
    Rec3 = #set_player_team{
        team_name = "red_team",
        method = 3,
        entities = ["Alex", "Steve"]
    },
    Meth3Bin = encode:encode_type(3, byte),
    Ent3Bin = encode:encode_type(["Alex", "Steve"], {prefixed_array, string}),
    Expected3Bin = <<Name1Bin/binary, Meth3Bin/binary, Ent3Bin/binary>>,
    ?assertEqual(Expected3Bin, encode:encode_type(Rec3, set_player_team)),

    %% Method 4 (Remove entities)
    Rec4 = #set_player_team{
        team_name = "red_team",
        method = 4,
        entities = ["Alex"]
    },
    Meth4Bin = encode:encode_type(4, byte),
    Ent4Bin = encode:encode_type(["Alex"], {prefixed_array, string}),
    Expected4Bin = <<Name1Bin/binary, Meth4Bin/binary, Ent4Bin/binary>>,
    ?assertEqual(Expected4Bin, encode:encode_type(Rec4, set_player_team)).

waypoint_data_test() ->
    %% 0: Empty
    Rec0 = #waypoint_data{waypoint_type = 0},
    Expected0Bin = <<0:8>>,
    ?assertEqual(Expected0Bin, encode:encode_type(Rec0, waypoint_data)),

    %% 1: Vec3i
    Rec1 = #waypoint_data{waypoint_type = 1, x = 100, y = 64, z = -200},
    Type1Bin = encode:encode_type(1, {enum, varint}),
    X1Bin = encode:encode_type(100, varint),
    Y1Bin = encode:encode_type(64, varint),
    Z1Bin = encode:encode_type(-200, varint),
    Expected1Bin = <<Type1Bin/binary, X1Bin/binary, Y1Bin/binary, Z1Bin/binary>>,
    ?assertEqual(Expected1Bin, encode:encode_type(Rec1, waypoint_data)),

    %% 2: Chunk
    Rec2 = #waypoint_data{waypoint_type = 2, x = 15, z = 30},
    Type2Bin = encode:encode_type(2, {enum, varint}),
    X2Bin = encode:encode_type(15, varint),
    Z2Bin = encode:encode_type(30, varint),
    Expected2Bin = <<Type2Bin/binary, X2Bin/binary, Z2Bin/binary>>,
    ?assertEqual(Expected2Bin, encode:encode_type(Rec2, waypoint_data)),

    %% 3: Azimuth
    Rec3 = #waypoint_data{waypoint_type = 3, angle = 1.57},
    Type3Bin = encode:encode_type(3, {enum, varint}),
    AngleBin = encode:encode_type(1.57, float),
    Expected3Bin = <<Type3Bin/binary, AngleBin/binary>>,
    ?assertEqual(Expected3Bin, encode:encode_type(Rec3, waypoint_data)).

stop_sound_test() ->
    %% 0: Neither source nor sound
    Rec0 = #stop_sound{flags = 0},
    Expected0Bin = <<0:8>>,
    ?assertEqual(Expected0Bin, encode:encode_type(Rec0, stop_sound)),

    %% 1: Source only
    Rec1 = #stop_sound{source = master},
    Flags1Bin = <<1:8>>,
    Source1Bin = encode:encode_type(0, {enum, varint}),
    Expected1Bin = <<Flags1Bin/binary, Source1Bin/binary>>,
    ?assertEqual(Expected1Bin, encode:encode_type(Rec1, stop_sound)),

    %% 2: Sound only
    Rec2 = #stop_sound{sound = <<"minecraft:ambient.cave">>},
    Flags2Bin = <<2:8>>,
    Sound2Bin = encode:encode_type(<<"minecraft:ambient.cave">>, identifier),
    Expected2Bin = <<Flags2Bin/binary, Sound2Bin/binary>>,
    ?assertEqual(Expected2Bin, encode:encode_type(Rec2, stop_sound)),

    %% 3: Source and Sound
    Rec3 = #stop_sound{source = player, sound = <<"minecraft:entity.generic.explode">>},
    Flags3Bin = <<3:8>>,
    Source3Bin = encode:encode_type(7, {enum, varint}),
    Sound3Bin = encode:encode_type(<<"minecraft:entity.generic.explode">>, identifier),
    Expected3Bin = <<Flags3Bin/binary, Source3Bin/binary, Sound3Bin/binary>>,
    ?assertEqual(Expected3Bin, encode:encode_type(Rec3, stop_sound)).

set_score_test() ->
    %% Minimal set_score with optional fields undefined
    Rec0 = #set_score{
        entity_name = "Player1",
        objective_name = "kills",
        value = 42
    },
    EntityBin = encode:encode_type("Player1", string),
    ObjectiveBin = encode:encode_type("kills", string),
    ValueBin = encode:encode_type(42, varint),
    NoDNBin = encode:encode_type(false, bool),
    NoNFBin = encode:encode_type(false, bool),
    Expected0Bin = <<EntityBin/binary, ObjectiveBin/binary, ValueBin/binary, NoDNBin/binary, NoNFBin/binary>>,
    ?assertEqual(Expected0Bin, encode:encode_type(Rec0, set_score)),

    %% set_score with display_name and styled number_format
    DNVal = #{type => <<"text">>, text => <<"Player One">>},
    StylingNBT = [{tag_compound, "", [{tag_string, "color", "red"}]}],
    Rec1 = #set_score{
        entity_name = "Player1",
        objective_name = "kills",
        value = 100,
        display_name = DNVal,
        number_format = {styled, StylingNBT}
    },
    DNBin = encode:encode_type(DNVal, text_component),
    HasDNBin = encode:encode_type(true, bool),
    HasNFBin = encode:encode_type(true, bool),
    NFIdBin = encode:encode_type(1, {enum, varint}),
    StylingBin = encode:encode_type(StylingNBT, nbt),
    Expected1Bin = <<EntityBin/binary, ObjectiveBin/binary, (encode:encode_type(100, varint))/binary, HasDNBin/binary, DNBin/binary, HasNFBin/binary, NFIdBin/binary, StylingBin/binary>>,
    ?assertEqual(Expected1Bin, encode:encode_type(Rec1, set_score)).

player_send_message_after_test() ->
    player:send_message_after(5, 'test_pkt_1', rec1),
    receive
        Msg1 -> ?assertEqual({send_message, 'test_pkt_1', rec1}, Msg1)
    after 100 ->
        ?assert(false)
    end,

    player:send_message_after('test_pkt_2', rec2, state),
    receive
        Msg2 -> ?assertEqual({send_message, 'test_pkt_2', rec2}, Msg2)
    after 100 ->
        ?assert(false)
    end,

    player:send_message_after(5, 'test_pkt_3', rec3, state),
    receive
        Msg3 -> ?assertEqual({send_message, 'test_pkt_3', rec3}, Msg3)
    after 100 ->
        ?assert(false)
    end.

select_known_packs_encode_test() ->
    Rec = #'minecraft:select_known_packs'{known_packs = [["minecraft", "core", "1.21"]]},
    {Enc, _} = player:encode_message('minecraft:select_known_packs', Rec, undefined),
    ?assertEqual({'minecraft:select_known_packs', [[["minecraft", "core", "1.21"]]]}, Enc).

set_chunk_cache_center_encode_test() ->
    Rec = #'minecraft:set_chunk_cache_center'{chunk_x = 5, chunk_z = -10},
    {Enc, _} = player:encode_message('minecraft:set_chunk_cache_center', Rec, undefined),
    ?assertEqual({'minecraft:set_chunk_cache_center', [5, -10]}, Enc),
    EncodedBytes = encode:encode_message(Enc, 'minecraft:set_chunk_cache_center', 5),
    ?assertEqual(<<94, 5, 246, 255, 255, 255, 15>>, EncodedBytes).














