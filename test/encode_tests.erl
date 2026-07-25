-module(encode_tests).
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
    %% Non-empty, no components: ItemCount=1, ItemID=5, NAdd=0, NRemove=0
    ?assertEqual(<<1, 5, 0, 0>>, encode:encode_type({1, 5, [], []}, slot)),
    %% With one add-component {TypeId=3, Data=<<1,2>>} and one remove TypeId=7:
    %% ItemCount=2, ItemID=10, NAdd=1, NRemove=1,
    %% add: TypeId=3, DataLen=2, Data=<<1,2>>, remove: TypeId=7
    ?assertEqual(<<2, 10, 1, 1, 3, 2, 1, 2, 7>>,
                 encode:encode_type({2, 10, [{3, <<1, 2>>}], [7]}, slot)).

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
