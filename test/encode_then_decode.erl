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
    {2147483647, <<>>} = decode:decode_type(encode:encode_type(2147483647, varint), varint),
    {-1, <<>>} = decode:decode_type(encode:encode_type(-1, varint), varint),
    {-2147483648, <<>>} = decode:decode_type(encode:encode_type(-2147483648, varint), varint).

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
