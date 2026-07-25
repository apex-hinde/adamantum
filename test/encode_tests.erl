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
    ?assertEqual(<<255, 255, 255, 255, 15>>, encode:encode_type(-1, varint)).

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

