-module(decode_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/data_types/records.hrl").
-include("src/data_types/components/component_records.hrl").

bool_test() ->
    ?assertEqual({<<>>, #bool{bool = true}}, decode:decode_type(<<1>>, bool)),
    ?assertEqual({<<>>, #bool{bool = false}}, decode:decode_type(<<0>>, bool)),
    ?assertEqual({<<"extra">>, #bool{bool = true}}, decode:decode_type(<<1, "extra">>, bool)).

byte_test() ->
    ?assertEqual({<<>>, #byte{byte = 127}}, decode:decode_type(<<127>>, byte)),
    ?assertEqual({<<>>, #byte{byte = -128}}, decode:decode_type(<<128>>, byte)),
    ?assertEqual({<<"rest">>, #byte{byte = 42}}, decode:decode_type(<<42, "rest">>, byte)).

ubyte_test() ->
    ?assertEqual({<<>>, #ubyte{ubyte = 255}}, decode:decode_type(<<255>>, ubyte)),
    ?assertEqual({<<>>, #ubyte{ubyte = 0}}, decode:decode_type(<<0>>, ubyte)),
    ?assertEqual({<<"rest">>, #ubyte{ubyte = 200}}, decode:decode_type(<<200, "rest">>, ubyte)).

short_test() ->
    ?assertEqual({<<>>, #short{short = 32767}}, decode:decode_type(<<32767:16/signed-integer>>, short)),
    ?assertEqual({<<>>, #short{short = -32768}}, decode:decode_type(<<-32768:16/signed-integer>>, short)),
    ?assertEqual({<<"rest">>, #short{short = 12345}}, decode:decode_type(<<12345:16/signed-integer, "rest">>, short)).

ushort_test() ->
    ?assertEqual({<<>>, #ushort{ushort = 65535}}, decode:decode_type(<<65535:16/unsigned-integer>>, ushort)),
    ?assertEqual({<<>>, #ushort{ushort = 0}}, decode:decode_type(<<0:16/unsigned-integer>>, ushort)),
    ?assertEqual({<<"rest">>, #ushort{ushort = 50000}}, decode:decode_type(<<50000:16/unsigned-integer, "rest">>, ushort)).

int_test() ->
    ?assertEqual({<<>>, #int{int = 2147483647}}, decode:decode_type(<<2147483647:32/signed-integer>>, int)),
    ?assertEqual({<<>>, #int{int = -2147483648}}, decode:decode_type(<<-2147483648:32/signed-integer>>, int)),
    ?assertEqual({<<"rest">>, #int{int = 1000000}}, decode:decode_type(<<1000000:32/signed-integer, "rest">>, int)).

long_test() ->
    Val = 9223372036854775807,
    ?assertEqual({<<>>, #long{long = Val}}, decode:decode_type(<<Val:64/signed-integer>>, long)),
    ?assertEqual({<<"rest">>, #long{long = -100}}, decode:decode_type(<<-100:64/signed-integer, "rest">>, long)).

float_test() ->
    Val = 3.14159,
    Input = <<Val:32/float, "rest">>,
    {Rest, #float{float = Decoded}} = decode:decode_type(Input, float),
    ?assertEqual(<<"rest">>, Rest),
    ?assert(abs(Val - Decoded) < 0.0001).

double_test() ->
    Val = 3.141592653589793,
    Input = <<Val:64/float, "rest">>,
    {Rest, #double{double = Decoded}} = decode:decode_type(Input, double),
    ?assertEqual(<<"rest">>, Rest),
    ?assertEqual(Val, Decoded).

string_test() ->
    Input = <<16, "Hello Minecraft!", "rest">>,
    ?assertEqual({<<"rest">>, #string{string = "Hello Minecraft!"}}, decode:decode_type(Input, string)).

varint_test() ->
    ?assertEqual({<<>>, #varint{varint = 0}}, decode:decode_type(<<0>>, varint)),
    ?assertEqual({<<>>, #varint{varint = 1}}, decode:decode_type(<<1>>, varint)),
    ?assertEqual({<<>>, #varint{varint = 127}}, decode:decode_type(<<127>>, varint)),
    ?assertEqual({<<>>, #varint{varint = 128}}, decode:decode_type(<<128, 1>>, varint)),
    ?assertEqual({<<>>, #varint{varint = 255}}, decode:decode_type(<<255, 1>>, varint)),
    ?assertEqual({<<"rest">>, #varint{varint = 300}}, decode:decode_type(<<172, 2, "rest">>, varint)),
    ?assertEqual({<<>>, #varint{varint = 2147483647}}, decode:decode_type(<<255, 255, 255, 255, 7>>, varint)),
    ?assertEqual({<<>>, #varint{varint = -1}}, decode:decode_type(<<255, 255, 255, 255, 15>>, varint)),
    ?assertEqual({<<>>, #varint{varint = -2147483648}}, decode:decode_type(<<128, 128, 128, 128, 8>>, varint)),
    ?assertEqual({<<>>, #varint{varint = 1}}, decode:decode_type(<<129, 0>>, varint)),

    ?assertEqual({error, "insufficient data"}, decode:decode_type(<<128>>, varint)),
    ?assertEqual({error, "varint too big"}, decode:decode_type(<<128, 128, 128, 128, 128, 1>>, varint)).

varlong_test() ->
    ?assertEqual({<<>>, #varlong{varlong = 0}}, decode:decode_type(<<0>>, varlong)),
    ?assertEqual({<<>>, #varlong{varlong = 1}}, decode:decode_type(<<1>>, varlong)),
    ?assertEqual({<<>>, #varlong{varlong = 127}}, decode:decode_type(<<127>>, varlong)),
    ?assertEqual({<<>>, #varlong{varlong = 128}}, decode:decode_type(<<128, 1>>, varlong)),
    ?assertEqual({<<>>, #varlong{varlong = 255}}, decode:decode_type(<<255, 1>>, varlong)),
    ?assertEqual({<<>>, #varlong{varlong = 2147483647}}, decode:decode_type(<<255, 255, 255, 255, 7>>, varlong)),
    ?assertEqual({<<>>, #varlong{varlong = 9223372036854775807}}, decode:decode_type(<<255, 255, 255, 255, 255, 255, 255, 255, 127>>, varlong)),
    ?assertEqual({<<>>, #varlong{varlong = -1}}, decode:decode_type(<<255, 255, 255, 255, 255, 255, 255, 255, 255, 1>>, varlong)),
    ?assertEqual({<<>>, #varlong{varlong = -9223372036854775808}}, decode:decode_type(<<128, 128, 128, 128, 128, 128, 128, 128, 128, 1>>, varlong)),
    ?assertEqual({<<>>, #varlong{varlong = 1}}, decode:decode_type(<<129, 0>>, varlong)),
    ?assertEqual({error, "insufficient data"}, decode:decode_type(<<128>>, varlong)),
    ?assertEqual({error, "varlong too big"}, decode:decode_type(<<128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 1>>, varlong)).

identifier_test() ->
    Input = <<17, "minecraft:diamond", "rest">>,
    ?assertEqual({<<"rest">>, #identifier{identifier = "minecraft:diamond"}}, decode:decode_type(Input, identifier)).

position_test() ->
    X = 100,
    Z = -200,
    Y = 50,
    Input = <<X:26/signed-integer, Z:26/signed-integer, Y:12/signed-integer, "rest">>,
    ?assertEqual({<<"rest">>, #position{x = X, y = Y, z = Z}}, decode:decode_type(Input, position)).

angle_test() ->
    ?assertEqual({<<"rest">>, #angle{angle = 45}}, decode:decode_type(<<45, "rest">>, angle)),
    ?assertEqual({<<"rest">>, #angle{angle = -128}}, decode:decode_type(<<128, "rest">>, angle)).

uuid_test() ->
    UUID = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    Input = <<UUID/binary, "rest">>,
    ?assertEqual({<<"rest">>, #uuid{uuid = UUID}}, decode:decode_type(Input, uuid)).

bitset_test() ->
    Input1 = <<1, 16#FF, "rest">>,
    ?assertEqual({<<"rest">>, #bitset{bitset = -1}}, decode:decode_type(Input1, bitset)),
    Input2 = <<2, 1, 0, "rest">>,
    ?assertEqual({<<"rest">>, #bitset{bitset = 256}}, decode:decode_type(Input2, bitset)).

fixed_bitset_test() ->
    Input1 = <<8, 16#FF, "rest">>,
    ?assertEqual({<<"rest">>, #fixed_bitset{fixed_bitset = -1}}, decode:decode_type(Input1, fixed_bitset)),
    Input2 = <<16, 1, 0, "rest">>,
    ?assertEqual({<<"rest">>, #fixed_bitset{fixed_bitset = 256}}, decode:decode_type(Input2, fixed_bitset)).

optional_test() ->
    ?assertEqual({<<"rest">>, #optional{some = some, optional = #int{int = 42}}}, decode:decode_type(<<42:32/signed-integer, "rest">>, {optional, int, true})),
    ?assertEqual({<<42:32/signed-integer, "rest">>, #optional{some = none, optional = none}}, decode:decode_type(<<42:32/signed-integer, "rest">>, {optional, int, false})),
    ?assertEqual({<<"rest">>, #optional{some = some, optional = #string{string = "hello"}}}, decode:decode_type(<<5, "hello", "rest">>, {optional, string, true})),
    ?assertEqual({<<"rest">>, #optional{some = none, optional = none}}, decode:decode_type(<<"rest">>, {optional, string, false})).

prefixed_optional_test() ->
    ?assertEqual({<<"rest">>, #prefixed_optional{some = some, prefixed_optional = #int{int = 42}}}, decode:decode_type(<<1, 42:32/signed-integer, "rest">>, {optional, int})),
    ?assertEqual({<<"rest">>, #prefixed_optional{some = none, prefixed_optional = none}}, decode:decode_type(<<0, "rest">>, {optional, int})).

id_or_x_test() ->
    ?assertEqual({<<"rest">>, #id_or_x{id_or_x = 0}}, decode:decode_type(<<1, "rest">>, id_or_x)),
    ?assertEqual({<<"rest">>, #id_or_x{id_or_x = 5}}, decode:decode_type(<<6, "rest">>, {id_or_x, string})),
    ?assertEqual({<<"rest">>, #id_or_x{id_or_x = 127}}, decode:decode_type(<<128, 1, "rest">>, {id_or_x, int})),
    ?assertEqual({<<"rest">>, #id_or_x{id_or_x = #string{string = "hello"}}}, decode:decode_type(<<0, 5, "hello", "rest">>, {id_or_x, string})),
    ?assertEqual({<<"rest">>, #id_or_x{id_or_x = #int{int = 42}}}, decode:decode_type(<<0, 42:32/signed-integer, "rest">>, {id_or_x, int})),
    ?assertEqual({error, "insufficient data"}, decode:decode_type(<<0>>, {id_or_x, varint})).

id_set_test() ->
    ?assertEqual({<<"rest">>, #id_set{id_set = "minecraft:wool"}}, decode:decode_type(<<0, 14, "minecraft:wool", "rest">>, id_set)),
    ?assertEqual({<<"rest">>, #id_set{id_set = [1, 2, 3]}}, decode:decode_type(<<4, 1, 2, 3, "rest">>, id_set)),
    ?assertEqual({<<"rest">>, #id_set{id_set = []}}, decode:decode_type(<<1, "rest">>, id_set)),
    ?assertEqual({error, "insufficient data"}, decode:decode_type(<<>>, id_set)).

array_test() ->
    ?assertEqual({<<"rest">>, #array{array = [#bool{bool = true}, #bool{bool = false}]}}, decode:decode_type(<<1, 0, "rest">>, {array, 2, bool})),
    ?assertEqual({<<"rest">>, #array{array = [#int{int = 1}, #int{int = 2}]}}, decode:decode_type(<<1:32/signed-integer, 2:32/signed-integer, "rest">>, {array, int, 2})).

prefixed_array_test() ->
    ?assertEqual({<<"rest">>, #prefixed_array{prefixed_array = [#byte{byte = 10}, #byte{byte = 20}]}}, decode:decode_type(<<2, 10, 20, "rest">>, {prefixed_array, byte})),
    ?assertEqual({<<"rest">>, #prefixed_array{prefixed_array = []}}, decode:decode_type(<<0, "rest">>, {prefixed_array, byte})),
    ?assertEqual({<<"rest">>, #prefixed_array{prefixed_array = [#int{int = 100}, #int{int = 200}]}}, decode:decode_type(<<2, 100:32/signed-integer, 200:32/signed-integer, "rest">>, {prefixed_array, int})),
    ?assertEqual({<<"rest">>, #prefixed_array{prefixed_array = [#byte{byte = 10}, #byte{byte = 20}]}}, decode:decode_type(<<0, 2, 10, 20, "rest">>, {prefixed_array, short, byte})).

enum_test() ->
    ?assertEqual({<<"rest">>, #enum{enum = 42}}, decode:decode_type(<<42, "rest">>, enum)),
    ?assertEqual({<<"rest">>, #enum{enum = 42}}, decode:decode_type(<<42, "rest">>, {enum, varint})),
    ?assertEqual({<<"rest">>, #enum{enum = 42}}, decode:decode_type(<<42, "rest">>, {enum, byte})),
    ?assertEqual({<<"rest">>, #enum{enum = 42}}, decode:decode_type(<<42:32/signed-integer, "rest">>, {enum, int})),
    ?assertEqual({<<"rest">>, #enum{enum = south}}, decode:decode_type(<<1, "rest">>, {enum, [north, south, east, west]})),
    ?assertEqual({<<"rest">>, #enum{enum = east}}, decode:decode_type(<<2, "rest">>, {enum, byte, [north, south, east, west]})),
    ?assertEqual({error, "invalid enum value"}, decode:decode_type(<<99, "rest">>, {enum, [north, south, east, west]})).

byte_array_test() ->
    ?assertEqual({<<>>, #byte_array{byte_array = <<"hello">>}}, decode:decode_type(<<"hello">>, byte_array)),
    ?assertEqual({<<"world">>, #byte_array{byte_array = <<"hello">>}}, decode:decode_type(<<"helloworld">>, {byte_array, 5})),
    ?assertEqual({<<"world">>, #byte_array{byte_array = <<"hello">>}}, decode:decode_type(<<5, "helloworld">>, {byte_array, varint})),
    ?assertEqual({<<"world">>, #byte_array{byte_array = <<"hello">>}}, decode:decode_type(<<0, 5, "helloworld">>, {byte_array, short})).

slot_test() ->
    %% Empty slot
    ?assertEqual({<<>>, #slot{item_count = 0, itemID = undefined, components_to_add = [], components_to_remove = []}}, decode:decode_type(<<0>>, slot)),
    ?assertEqual({<<"rest">>, #slot{item_count = 0, itemID = undefined, components_to_add = [], components_to_remove = []}}, decode:decode_type(<<0, "rest">>, slot)),
    %% Non-empty, no components
    ?assertEqual({<<>>, #slot{item_count = 1, itemID = 5, components_to_add = [], components_to_remove = []}},
                 decode:decode_type(<<1, 5, 0, 0>>, slot)),
    %% With one add component (TypeId=3 minecraft:damage with damage=2) and one remove TypeId=7
    Input1 = <<2, 10, 1, 1, 3, 2, 7>>,
    Expected1 = #slot{item_count = 2, itemID = 10,
                      components_to_add = [{3, #damage{type = 'minecraft:damage', damage = 2}}],
                      components_to_remove = [7]},
    ?assertEqual({<<>>, Expected1}, decode:decode_type(Input1, slot)),
    %% Multiple add components (max_stack_size and unbreakable) and multiple remove components, with trailing rest data
    Input2 = <<5, 20, 2, 2, 1, 64, 4, 2, 5, "rest">>,
    Expected2 = #slot{item_count = 5, itemID = 20,
                      components_to_add = [
                          {1, #max_stack_size{type = 'minecraft:max_stack_size', max_stack_size = 64}},
                          {4, #unbreakable{type = 'minecraft:unbreakable'}}
                      ],
                      components_to_remove = [2, 5]},
    ?assertEqual({<<"rest">>, Expected2}, decode:decode_type(Input2, slot)).

hashed_slot_test() ->
    %% Empty hashed slot
    ?assertEqual({<<>>, #hashed_slot{item_count = 0, itemID = undefined, components_to_add = [], components_to_remove = []}}, decode:decode_type(<<0>>, hashed_slot)),
    ?assertEqual({<<"rest">>, #hashed_slot{item_count = 0, itemID = undefined, components_to_add = [], components_to_remove = []}}, decode:decode_type(<<0, "rest">>, hashed_slot)),
    %% Non-empty, no components
    ?assertEqual({<<>>, #hashed_slot{item_count = 1, itemID = 5, components_to_add = [], components_to_remove = []}},
                 decode:decode_type(<<1, 5, 1, 0, 0>>, hashed_slot)),
    %% With one add {TypeId=3, Hash=12345} and one remove TypeId=7
    Input = <<1, 5, 1, 1, 3, 12345:32/signed-integer, 1, 7>>,
    ?assertEqual({<<>>, #hashed_slot{item_count = 1, itemID = 5, components_to_add = [{3, 12345}], components_to_remove = [7]}},
                 decode:decode_type(Input, hashed_slot)).

text_component_test() ->
    SNBTBin = <<"{text: \"Hello\", color: \"red\"}">>,
    LenBin = encode:encode_type(byte_size(SNBTBin), varint),
    {Rest, Comp} = decode:decode_type(<<LenBin/binary, SNBTBin/binary, "rest">>, text_component),
    ?assertEqual(<<"rest">>, Rest),
    Map = Comp#text_component.component_map,
    ?assertEqual(<<"text">>, maps:get(type, Map)),
    ?assertEqual(<<"Hello">>, maps:get(text, Map)),
    ?assertEqual(<<"red">>, maps:get(color, Map)).

json_text_component_test() ->
    JsonBin = <<"{\"text\":\"Hello\",\"color\":\"red\"}">>,
    LenBin = encode:encode_type(byte_size(JsonBin), varint),
    {Rest, Comp} = decode:decode_type(<<LenBin/binary, JsonBin/binary, "rest">>, json_text_component),
    ?assertEqual(<<"rest">>, Rest),
    Map = Comp#json_text_component.json_component_map,
    ?assertEqual(<<"Hello">>, maps:get(<<"text">>, Map)),
    ?assertEqual(<<"red">>, maps:get(<<"color">>, Map)).

teleport_flags_test() ->
    %% All flags false (0)
    {Rest1, Rec1} = decode:decode_type(<<0:32/signed-integer, "rest">>, teleport_flags),
    Flags1 = Rec1#teleport_flags.flagsmap,
    ?assertEqual(<<"rest">>, Rest1),
    ?assertEqual(false, maps:get(relative_x, Flags1)),
    ?assertEqual(false, maps:get(relative_y, Flags1)),
    ?assertEqual(false, maps:get(relative_z, Flags1)),
    ?assertEqual(false, maps:get(relative_yaw, Flags1)),
    ?assertEqual(false, maps:get(relative_pitch, Flags1)),
    ?assertEqual(false, maps:get(relative_velocity_x, Flags1)),
    ?assertEqual(false, maps:get(relative_velocity_y, Flags1)),
    ?assertEqual(false, maps:get(relative_velocity_z, Flags1)),
    ?assertEqual(false, maps:get(rotate_velocity, Flags1)),

    %% Flags relative_x (0x0001), relative_z (0x0004), rotate_velocity (0x0100) -> 0x0105
    IntVal = 16#0001 bor 16#0004 bor 16#0100,
    {Rest2, Rec2} = decode:decode_type(<<IntVal:32/signed-integer, "rest">>, teleport_flags),
    Flags2 = Rec2#teleport_flags.flagsmap,
    ?assertEqual(<<"rest">>, Rest2),
    ?assertEqual(true, maps:get(relative_x, Flags2)),
    ?assertEqual(false, maps:get(relative_y, Flags2)),
    ?assertEqual(true, maps:get(relative_z, Flags2)),
    ?assertEqual(false, maps:get(relative_yaw, Flags2)),
    ?assertEqual(false, maps:get(relative_pitch, Flags2)),
    ?assertEqual(false, maps:get(relative_velocity_x, Flags2)),
    ?assertEqual(false, maps:get(relative_velocity_y, Flags2)),
    ?assertEqual(false, maps:get(relative_velocity_z, Flags2)),
    ?assertEqual(true, maps:get(rotate_velocity, Flags2)),

    %% All bits 0x01FF set -> all true
    AllFlags = 16#01FF,
    {Rest3, Rec3} = decode:decode_type(<<AllFlags:32/signed-integer, "rest">>, teleport_flags),
    Flags3 = Rec3#teleport_flags.flagsmap,
    ?assertEqual(<<"rest">>, Rest3),
    ?assertEqual(true, maps:get(relative_x, Flags3)),
    ?assertEqual(true, maps:get(relative_y, Flags3)),
    ?assertEqual(true, maps:get(relative_z, Flags3)),
    ?assertEqual(true, maps:get(relative_yaw, Flags3)),
    ?assertEqual(true, maps:get(relative_pitch, Flags3)),
    ?assertEqual(true, maps:get(relative_velocity_x, Flags3)),
    ?assertEqual(true, maps:get(relative_velocity_y, Flags3)),
    ?assertEqual(true, maps:get(relative_velocity_z, Flags3)),
    ?assertEqual(true, maps:get(rotate_velocity, Flags3)).

either_x_or_y_test() ->
    %% Boolean true (1) -> decodes X (byte)
    ?assertEqual({<<"rest">>, #either_x_or_y{x = #byte{byte = 42}, y = undefined}}, decode:decode_type(<<1, 42, "rest">>, {either_x_or_y, byte, string})),
    %% Boolean false (0) -> decodes Y (string)
    ?assertEqual({<<"rest">>, #either_x_or_y{x = undefined, y = #string{string = "hello"}}}, decode:decode_type(<<0, 5, "hello", "rest">>, {either_x_or_y, byte, string})).

game_profile_test() ->
    UUIDBin = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    UsernameBin = <<7, "Player1">>,
    CountBin = <<2>>,
    Prop1Bin = <<8, "textures", 4, "val1", 1, 4, "sig1">>,
    Prop2Bin = <<4, "cape", 4, "val2", 0>>,
    Input = <<UUIDBin/binary, UsernameBin/binary, CountBin/binary, Prop1Bin/binary, Prop2Bin/binary, "rest">>,
    Expected = #game_profile{uuid = UUIDBin, username = "Player1", properties = [
        {"textures", "val1", #prefixed_optional{some = some, prefixed_optional = #string{string = "sig1"}}},
        {"cape", "val2", #prefixed_optional{some = none, prefixed_optional = none}}
    ]},
    ?assertEqual({<<"rest">>, Expected}, decode:decode_type(Input, game_profile)).

resolvable_profile_partial_test() ->
    %% ProfileKind = 0 (Partial)
    ProfileKindBin = <<0>>,
    UserBin = <<1, 5, "Steve">>,
    UuidBin = <<0>>,
    CountBin = <<1>>,
    Prop1Bin = <<8, "textures", 8, "tex_data", 0>>,
    BodyBin = <<14, "textures/skin1">>,
    ModelBin = <<1>>,
    Input = <<ProfileKindBin/binary, UserBin/binary, UuidBin/binary, CountBin/binary, Prop1Bin/binary, BodyBin/binary, ModelBin/binary, "rest">>,
    TypeSpec = {resolvable_profile, true, false, false, true},
    ExpectedProfile = {#prefixed_optional{some = some, prefixed_optional = #string{string = "Steve"}}, #prefixed_optional{some = none, prefixed_optional = none}, [{"textures", "tex_data", #prefixed_optional{some = none, prefixed_optional = none}}]},
    Expected = #resolvable_profile{profile_kind = 0, profile = ExpectedProfile, body = #optional{some = some, optional = #identifier{identifier = "textures/skin1"}}, cape = #optional{some = none, optional = none}, elytra = #optional{some = none, optional = none}, model = #optional{some = some, optional = #varint{varint = 1}}},
    ?assertEqual({<<"rest">>, Expected}, decode:decode_type(Input, TypeSpec)).

resolvable_profile_complete_test() ->
    %% ProfileKind = 1 (Complete)
    ProfileKindBin = <<1>>,
    UUID = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    UserBin = <<4, "Alex">>,
    CountBin = <<0>>,
    Input = <<ProfileKindBin/binary, UUID/binary, UserBin/binary, CountBin/binary, "rest">>,
    TypeSpec = {resolvable_profile, false, false, false, false},
    ExpectedProfile = #game_profile{uuid = UUID, username = "Alex", properties = []},
    Expected = #resolvable_profile{profile_kind = 1, profile = ExpectedProfile, body = #optional{some = none, optional = none}, cape = #optional{some = none, optional = none}, elytra = #optional{some = none, optional = none}, model = #optional{some = none, optional = none}},
    ?assertEqual({<<"rest">>, Expected}, decode:decode_type(Input, TypeSpec)).

lp_vec3_test() ->
    ?assertEqual({<<>>, #lp_vec3{x = 0.0, y = 0.0, z = 0.0}}, decode:decode_type(<<0>>, lp_vec3)),
    ?assertEqual({<<"rest">>, #lp_vec3{x = 0.0, y = 0.0, z = 0.0}}, decode:decode_type(<<0, "rest">>, lp_vec3)),
    
    Sample2Bin = <<241, 255, 0, 0, 255, 255, "rest">>,
    {Rest2, #lp_vec3{x = X2, y = Y2, z = Z2}} = decode:decode_type(Sample2Bin, lp_vec3),
    ?assertEqual(<<"rest">>, Rest2),
    ?assert(abs(X2 - 1.0) < 0.001),
    ?assert(abs(Y2 - 0.0) < 0.001),
    ?assert(abs(Z2 - (-1.0)) < 0.001),

    Sample3Bin = <<246, 255, 64, 1, 5, 31, 2, "rest">>,
    {Rest3, #lp_vec3{x = X3, y = Y3, z = Z3}} = decode:decode_type(Sample3Bin, lp_vec3),
    ?assertEqual(<<"rest">>, Rest3),
    ?assert(abs(X3 - 10.0) < 0.05),
    ?assert(abs(Y3 - 0.2) < 0.05),
    ?assert(abs(Z3 - (-5.0)) < 0.05),

    Sample4Bin = <<245, 255, 127, 255, 0, 7, 144, 241, 1, "rest">>,
    {Rest4, #lp_vec3{x = X4, y = Y4, z = Z4}} = decode:decode_type(Sample4Bin, lp_vec3),
    ?assertEqual(<<"rest">>, Rest4),
    ?assert(abs(X4 - 123457.0) < 1.0),
    ?assert(abs(Y4 - 15.071) < 0.1),
    ?assert(abs(Z4 - 0.0) < 0.05),

    ?assertEqual({error, "insufficient data"}, decode:decode_type(<<>>, lp_vec3)),
    ?assertEqual({error, "insufficient data"}, decode:decode_type(<<246, 255, 64, 1, 5, 31>>, lp_vec3)).

set_equipment_test() ->
    EmptySlot = #slot{item_count = 0, itemID = undefined, components_to_add = [], components_to_remove = []},
    Bin = <<1, 16#80, 0, 1, 0, "rest">>,
    {Rest, Rec} = decode:decode_type(Bin, set_equipment),
    ?assertEqual(<<"rest">>, Rest),
    ?assertEqual(#varint{varint = 1}, Rec#set_equipment.entity_id),
    ?assertEqual([
        {#enum{enum = 0}, EmptySlot},
        {#enum{enum = 1}, EmptySlot}
    ], Rec#set_equipment.equipment).

set_objective_test() ->
    %% Mode 1: Remove objective
    Mode1Rec = #set_objective{
        objective_name = "test_obj",
        mode = 1,
        objective_value = undefined,
        type = undefined,
        number_format = undefined
    },
    Mode1Bin = encode:encode_type(Mode1Rec, set_objective),
    {<<>>, DecMode1} = decode:decode_type(Mode1Bin, set_objective),
    ?assertEqual("test_obj", DecMode1#set_objective.objective_name),
    ?assertEqual(1, DecMode1#set_objective.mode),

    %% Mode 0: Create objective (no number format)
    Mode0Rec = #set_objective{
        objective_name = "obj_0",
        mode = 0,
        objective_value = #{type => <<"text">>, text => <<"Title">>},
        type = 0,
        number_format = undefined
    },
    Mode0Bin = encode:encode_type(Mode0Rec, set_objective),
    {<<>>, DecMode0} = decode:decode_type(Mode0Bin, set_objective),
    ?assertEqual("obj_0", DecMode0#set_objective.objective_name),
    ?assertEqual(0, DecMode0#set_objective.mode),
    ?assertEqual(0, DecMode0#set_objective.type),
    ?assertEqual(undefined, DecMode0#set_objective.number_format),

    %% Mode 2: Update display text (blank number format)
    Mode2Rec = #set_objective{
        objective_name = "obj_2",
        mode = 2,
        objective_value = #{type => <<"text">>, text => <<"Updated">>},
        type = 1,
        number_format = blank
    },
    Mode2Bin = encode:encode_type(Mode2Rec, set_objective),
    {<<>>, DecMode2} = decode:decode_type(Mode2Bin, set_objective),
    ?assertEqual("obj_2", DecMode2#set_objective.objective_name),
    ?assertEqual(2, DecMode2#set_objective.mode),
    ?assertEqual(1, DecMode2#set_objective.type),
    ?assertEqual(blank, DecMode2#set_objective.number_format).

set_player_team_test() ->
    %% Method 1: Remove team
    Meth1Rec = #set_player_team{
        team_name = "team_alpha",
        method = 1
    },
    Meth1Bin = encode:encode_type(Meth1Rec, set_player_team),
    {<<>>, DecMeth1} = decode:decode_type(Meth1Bin, set_player_team),
    ?assertEqual("team_alpha", DecMeth1#set_player_team.team_name),
    ?assertEqual(1, DecMeth1#set_player_team.method),

    %% Method 0: Create team
    Meth0Rec = #set_player_team{
        team_name = "team_alpha",
        method = 0,
        team_display_name = #{type => <<"text">>, text => <<"Alpha">>},
        team_prefix = #{type => <<"text">>, text => <<"[A] ">>},
        team_suffix = #{type => <<"text">>, text => <<"">>},
        name_tag_visibility = 0,
        collision_rule = 0,
        team_color = 12,
        friendly_flags = 1,
        entities = ["PlayerA"]
    },
    Meth0Bin = encode:encode_type(Meth0Rec, set_player_team),
    {<<>>, DecMeth0} = decode:decode_type(Meth0Bin, set_player_team),
    ?assertEqual("team_alpha", DecMeth0#set_player_team.team_name),
    ?assertEqual(0, DecMeth0#set_player_team.method),
    ?assertEqual(0, DecMeth0#set_player_team.name_tag_visibility),
    ?assertEqual(0, DecMeth0#set_player_team.collision_rule),
    ?assertEqual(12, DecMeth0#set_player_team.team_color),
    ?assertEqual(1, DecMeth0#set_player_team.friendly_flags),

    %% Method 2: Update team info
    Meth2Rec = #set_player_team{
        team_name = "team_alpha",
        method = 2,
        team_display_name = #{type => <<"text">>, text => <<"Alpha Team">>},
        team_prefix = #{type => <<"text">>, text => <<"[Alpha] ">>},
        team_suffix = #{type => <<"text">>, text => <<"">>},
        name_tag_visibility = 1,
        collision_rule = 2,
        team_color = 5,
        friendly_flags = 2
    },
    Meth2Bin = encode:encode_type(Meth2Rec, set_player_team),
    {<<>>, DecMeth2} = decode:decode_type(Meth2Bin, set_player_team),
    ?assertEqual("team_alpha", DecMeth2#set_player_team.team_name),
    ?assertEqual(2, DecMeth2#set_player_team.method),
    ?assertEqual(1, DecMeth2#set_player_team.name_tag_visibility),
    ?assertEqual(2, DecMeth2#set_player_team.collision_rule),
    ?assertEqual(5, DecMeth2#set_player_team.team_color),
    ?assertEqual(2, DecMeth2#set_player_team.friendly_flags),

    %% Method 3: Add entities
    Meth3Rec = #set_player_team{
        team_name = "team_alpha",
        method = 3,
        entities = ["PlayerB", "PlayerC"]
    },
    Meth3Bin = encode:encode_type(Meth3Rec, set_player_team),
    {<<>>, DecMeth3} = decode:decode_type(Meth3Bin, set_player_team),
    ?assertEqual("team_alpha", DecMeth3#set_player_team.team_name),
    ?assertEqual(3, DecMeth3#set_player_team.method),

    %% Method 4: Remove entities
    Meth4Rec = #set_player_team{
        team_name = "team_alpha",
        method = 4,
        entities = ["PlayerB"]
    },
    Meth4Bin = encode:encode_type(Meth4Rec, set_player_team),
    {<<>>, DecMeth4} = decode:decode_type(Meth4Bin, set_player_team),
    ?assertEqual("team_alpha", DecMeth4#set_player_team.team_name),
    ?assertEqual(4, DecMeth4#set_player_team.method).

waypoint_data_test() ->
    %% 0: Empty
    Rec0 = #waypoint_data{waypoint_type = 0},
    Bin0 = encode:encode_type(Rec0, waypoint_data),
    {<<>>, Dec0} = decode:decode_type(Bin0, waypoint_data),
    ?assertEqual(0, Dec0#waypoint_data.waypoint_type),

    %% 1: Vec3i
    Rec1 = #waypoint_data{waypoint_type = 1, x = 123, y = 45, z = -678},
    Bin1 = encode:encode_type(Rec1, waypoint_data),
    {<<>>, Dec1} = decode:decode_type(Bin1, waypoint_data),
    ?assertEqual(1, Dec1#waypoint_data.waypoint_type),
    ?assertEqual(123, Dec1#waypoint_data.x),
    ?assertEqual(45, Dec1#waypoint_data.y),
    ?assertEqual(-678, Dec1#waypoint_data.z),

    %% 2: Chunk
    Rec2 = #waypoint_data{waypoint_type = 2, x = 10, z = -20},
    Bin2 = encode:encode_type(Rec2, waypoint_data),
    {<<>>, Dec2} = decode:decode_type(Bin2, waypoint_data),
    ?assertEqual(2, Dec2#waypoint_data.waypoint_type),
    ?assertEqual(10, Dec2#waypoint_data.x),
    ?assertEqual(-20, Dec2#waypoint_data.z),

    %% 3: Azimuth
    Rec3 = #waypoint_data{waypoint_type = 3, angle = 3.14159},
    Bin3 = encode:encode_type(Rec3, waypoint_data),
    {<<>>, Dec3} = decode:decode_type(Bin3, waypoint_data),
    ?assertEqual(3, Dec3#waypoint_data.waypoint_type),
    ?assert(abs(3.14159 - Dec3#waypoint_data.angle) < 0.0001).

stop_sound_test() ->
    %% 0: Neither source nor sound
    Rec0 = #stop_sound{flags = 0},
    Bin0 = encode:encode_type(Rec0, stop_sound),
    {<<>>, Dec0} = decode:decode_type(Bin0, stop_sound),
    ?assertEqual(0, Dec0#stop_sound.flags),
    ?assertEqual(undefined, Dec0#stop_sound.source),
    ?assertEqual(undefined, Dec0#stop_sound.sound),

    %% 1: Source only
    Rec1 = #stop_sound{source = master},
    Bin1 = encode:encode_type(Rec1, stop_sound),
    {<<>>, Dec1} = decode:decode_type(Bin1, stop_sound),
    ?assertEqual(1, Dec1#stop_sound.flags),
    ?assertEqual(0, Dec1#stop_sound.source),
    ?assertEqual(undefined, Dec1#stop_sound.sound),

    %% 2: Sound only
    Rec2 = #stop_sound{sound = <<"minecraft:ambient.cave">>},
    Bin2 = encode:encode_type(Rec2, stop_sound),
    {<<>>, Dec2} = decode:decode_type(Bin2, stop_sound),
    ?assertEqual(2, Dec2#stop_sound.flags),
    ?assertEqual(undefined, Dec2#stop_sound.source),
    ?assertEqual("minecraft:ambient.cave", Dec2#stop_sound.sound),

    %% 3: Source and Sound
    Rec3 = #stop_sound{source = 7, sound = <<"minecraft:entity.generic.explode">>},
    Bin3 = encode:encode_type(Rec3, stop_sound),
    {<<>>, Dec3} = decode:decode_type(Bin3, stop_sound),
    ?assertEqual(3, Dec3#stop_sound.flags),
    ?assertEqual(7, Dec3#stop_sound.source),
    ?assertEqual("minecraft:entity.generic.explode", Dec3#stop_sound.sound).

set_score_test() ->
    %% Minimal set_score
    Rec0 = #set_score{
        entity_name = "Player1",
        objective_name = "kills",
        value = 42
    },
    Bin0 = encode:encode_type(Rec0, set_score),
    {<<>>, Dec0} = decode:decode_type(Bin0, set_score),
    ?assertEqual("Player1", Dec0#set_score.entity_name),
    ?assertEqual("kills", Dec0#set_score.objective_name),
    ?assertEqual(42, Dec0#set_score.value),
    ?assertEqual(undefined, Dec0#set_score.display_name),
    ?assertEqual(undefined, Dec0#set_score.number_format),

    %% set_score with display_name and blank number_format
    DNVal = #{type => <<"text">>, text => <<"Player One">>},
    Rec1 = #set_score{
        entity_name = "Player1",
        objective_name = "kills",
        value = 100,
        display_name = DNVal,
        number_format = blank
    },
    Bin1 = encode:encode_type(Rec1, set_score),
    {<<>>, Dec1} = decode:decode_type(Bin1, set_score),
    ?assertEqual("Player1", Dec1#set_score.entity_name),
    ?assertEqual("kills", Dec1#set_score.objective_name),
    ?assertEqual(100, Dec1#set_score.value),
    ?assertEqual(blank, Dec1#set_score.number_format),

    %% set_score with fixed number_format
    FixedContent = #{type => <<"text">>, text => <<"FixedVal">>},
    Rec2 = #set_score{
        entity_name = "Player2",
        objective_name = "deaths",
        value = 5,
        display_name = undefined,
        number_format = {fixed, FixedContent}
    },
    Bin2 = encode:encode_type(Rec2, set_score),
    {<<>>, Dec2} = decode:decode_type(Bin2, set_score),
    ?assertEqual("Player2", Dec2#set_score.entity_name),
    ?assertEqual("deaths", Dec2#set_score.objective_name),
    ?assertEqual(5, Dec2#set_score.value),
    ?assertEqual(undefined, Dec2#set_score.display_name),
    ?assertEqual({fixed, #text_component{component_map = FixedContent}}, Dec2#set_score.number_format).

custom_payload_decode_test() ->
    %% <<25, 2, 15, "minecraft:brand", 7, "vanilla">>
    %% Length = 25 (varint), Packet ID = 2 (varint)
    PayloadBin = <<15, "minecraft:brand", 7, "vanilla">>,
    Rec = decode:decode_message(PayloadBin, 'minecraft:custom_payload'),
    ?assertEqual("minecraft:brand", Rec#'minecraft:custom_payload'.channel),
    ?assertEqual(<<7, "vanilla">>, Rec#'minecraft:custom_payload'.data).

client_information_decode_test() ->
    %% Example message: <<0, 5, 101, 110, 95, 117, 115, 16, 0, 1, 127, 1, 1, 1, 0>> with packet id (0)
    %% Payload after packet ID 0: <<5, 101, 110, 95, 117, 115, 16, 0, 1, 127, 1, 1, 1, 0>>
    Data = <<5, 101, 110, 95, 117, 115, 16, 0, 1, 127, 1, 1, 1, 0>>,
    Expected = #'minecraft:client_information'{
        locale = "en_us",
        view_distance = 16,
        chat_mode = 0,
        chat_colors = true,
        displayed_skin_parts = 127,
        main_hand = 1,
        enable_text_filtering = true,
        allow_server_listings = true,
        particle_status = 0
    },
    Rec = decode:decode_message(Data, 'minecraft:client_information'),
    ?assertEqual(Expected, Rec),
    {ok, dummy_state} = decode_messages:decode_message({'minecraft:client_information', Rec}, dummy_state).





