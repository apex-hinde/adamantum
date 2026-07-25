-module(encode_then_decode).
-include_lib("eunit/include/eunit.hrl").

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

varint_test() ->
    {0, <<>>} = decode:decode_type(encode:encode_type(0, varint), varint),
    {128, <<>>} = decode:decode_type(encode:encode_type(128, varint), varint),
    {300, <<>>} = decode:decode_type(encode:encode_type(300, varint), varint),
    {2147483647, <<>>} = decode:decode_type(encode:encode_type(2147483647, varint), varint).

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

