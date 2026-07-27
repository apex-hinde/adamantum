-module(decode_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/data_types/records.hrl").

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
    %% With one add {TypeId=3, Data=<<1,2>>} and one remove TypeId=7
    Input = <<2, 10, 1, 1, 3, 2, 1, 2, 7>>,
    ?assertEqual({<<>>, #slot{item_count = 2, itemID = 10, components_to_add = [{3, <<1, 2>>}], components_to_remove = [7]}},
                 decode:decode_type(Input, slot)).

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
