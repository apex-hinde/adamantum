-module(decode_tests).
-include_lib("eunit/include/eunit.hrl").

bool_test() ->
    ?assertEqual({<<>>, true}, decode:decode_type(<<1>>, bool)),
    ?assertEqual({<<>>, false}, decode:decode_type(<<0>>, bool)),
    ?assertEqual({<<"extra">>, true}, decode:decode_type(<<1, "extra">>, bool)).

byte_test() ->
    ?assertEqual({<<>>, 127}, decode:decode_type(<<127>>, byte)),
    ?assertEqual({<<>>, -128}, decode:decode_type(<<128>>, byte)),
    ?assertEqual({<<"rest">>, 42}, decode:decode_type(<<42, "rest">>, byte)).

ubyte_test() ->
    ?assertEqual({<<>>, 255}, decode:decode_type(<<255>>, ubyte)),
    ?assertEqual({<<>>, 0}, decode:decode_type(<<0>>, ubyte)),
    ?assertEqual({<<"rest">>, 200}, decode:decode_type(<<200, "rest">>, ubyte)).

short_test() ->
    ?assertEqual({<<>>, 32767}, decode:decode_type(<<32767:16/signed-integer>>, short)),
    ?assertEqual({<<>>, -32768}, decode:decode_type(<<-32768:16/signed-integer>>, short)),
    ?assertEqual({<<"rest">>, 12345}, decode:decode_type(<<12345:16/signed-integer, "rest">>, short)).

ushort_test() ->
    ?assertEqual({<<>>, 65535}, decode:decode_type(<<65535:16/unsigned-integer>>, ushort)),
    ?assertEqual({<<>>, 0}, decode:decode_type(<<0:16/unsigned-integer>>, ushort)),
    ?assertEqual({<<"rest">>, 50000}, decode:decode_type(<<50000:16/unsigned-integer, "rest">>, ushort)).

int_test() ->
    ?assertEqual({<<>>, 2147483647}, decode:decode_type(<<2147483647:32/signed-integer>>, int)),
    ?assertEqual({<<>>, -2147483648}, decode:decode_type(<<-2147483648:32/signed-integer>>, int)),
    ?assertEqual({<<"rest">>, 1000000}, decode:decode_type(<<1000000:32/signed-integer, "rest">>, int)).

long_test() ->
    Val = 9223372036854775807,
    ?assertEqual({<<>>, Val}, decode:decode_type(<<Val:64/signed-integer>>, long)),
    ?assertEqual({<<"rest">>, -100}, decode:decode_type(<<-100:64/signed-integer, "rest">>, long)).

float_test() ->
    Val = 3.14159,
    Input = <<Val:32/float, "rest">>,
    {Rest, Decoded} = decode:decode_type(Input, float),
    ?assertEqual(<<"rest">>, Rest),
    ?assert(abs(Val - Decoded) < 0.0001).

double_test() ->
    Val = 3.141592653589793,
    Input = <<Val:64/float, "rest">>,
    {Rest, Decoded} = decode:decode_type(Input, double),
    ?assertEqual(<<"rest">>, Rest),
    ?assertEqual(Val, Decoded).

string_test() ->
    Input = <<16, "Hello Minecraft!", "rest">>,
    ?assertEqual({<<"rest">>, "Hello Minecraft!"}, decode:decode_type(Input, string)).

varint_test() ->
    ?assertEqual({0, <<>>}, decode:decode_type(<<0>>, varint)),
    ?assertEqual({1, <<>>}, decode:decode_type(<<1>>, varint)),
    ?assertEqual({127, <<>>}, decode:decode_type(<<127>>, varint)),
    ?assertEqual({128, <<>>}, decode:decode_type(<<128, 1>>, varint)),
    ?assertEqual({255, <<>>}, decode:decode_type(<<255, 1>>, varint)),
    ?assertEqual({300, <<"rest">>}, decode:decode_type(<<172, 2, "rest">>, varint)),
    ?assertEqual({2147483647, <<>>}, decode:decode_type(<<255, 255, 255, 255, 7>>, varint)),
    ?assertEqual({-1, <<>>}, decode:decode_type(<<255, 255, 255, 255, 15>>, varint)),
    ?assertEqual({-2147483648, <<>>}, decode:decode_type(<<128, 128, 128, 128, 8>>, varint)),
    ?assertEqual({1, <<>>}, decode:decode_type(<<129, 0>>, varint)),
    ?assertEqual({error, "insufficient data"}, decode:decode_type(<<128>>, varint)),
    ?assertEqual({error, "varint too big"}, decode:decode_type(<<128, 128, 128, 128, 128, 1>>, varint)).

varlong_test() ->
    ?assertEqual({0, <<>>}, decode:decode_type(<<0>>, varlong)),
    ?assertEqual({1, <<>>}, decode:decode_type(<<1>>, varlong)),
    ?assertEqual({127, <<>>}, decode:decode_type(<<127>>, varlong)),
    ?assertEqual({128, <<>>}, decode:decode_type(<<128, 1>>, varlong)),
    ?assertEqual({255, <<>>}, decode:decode_type(<<255, 1>>, varlong)),
    ?assertEqual({2147483647, <<>>}, decode:decode_type(<<255, 255, 255, 255, 7>>, varlong)),
    ?assertEqual({9223372036854775807, <<>>}, decode:decode_type(<<255, 255, 255, 255, 255, 255, 255, 255, 127>>, varlong)),
    ?assertEqual({-1, <<>>}, decode:decode_type(<<255, 255, 255, 255, 255, 255, 255, 255, 255, 1>>, varlong)),
    ?assertEqual({-9223372036854775808, <<>>}, decode:decode_type(<<128, 128, 128, 128, 128, 128, 128, 128, 128, 1>>, varlong)),
    ?assertEqual({1, <<>>}, decode:decode_type(<<129, 0>>, varlong)),
    ?assertEqual({error, "insufficient data"}, decode:decode_type(<<128>>, varlong)),
    ?assertEqual({error, "varlong too big"}, decode:decode_type(<<128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 1>>, varlong)).

identifier_test() ->
    Input = <<17, "minecraft:diamond", "rest">>,
    ?assertEqual({<<"rest">>, "minecraft:diamond"}, decode:decode_type(Input, identifier)).

position_test() ->
    X = 100,
    Z = -200,
    Y = 50,
    Input = <<X:26/signed-integer, Z:26/signed-integer, Y:12/signed-integer, "rest">>,
    ?assertEqual({<<"rest">>, {X, Z, Y}}, decode:decode_type(Input, position)).

angle_test() ->
    ?assertEqual({<<"rest">>, 45}, decode:decode_type(<<45, "rest">>, angle)),
    ?assertEqual({<<"rest">>, -128}, decode:decode_type(<<128, "rest">>, angle)).

uuid_test() ->
    UUID = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16>>,
    Input = <<UUID/binary, "rest">>,
    ?assertEqual({<<"rest">>, UUID}, decode:decode_type(Input, uuid)).

bitset_test() ->
    Input1 = <<1, 16#FF, "rest">>,
    ?assertEqual({<<"rest">>, -1}, decode:decode_type(Input1, bitset)),
    Input2 = <<2, 1, 0, "rest">>,
    ?assertEqual({<<"rest">>, 256}, decode:decode_type(Input2, bitset)).

fixed_bitset_test() ->
    Input1 = <<8, 16#FF, "rest">>,
    ?assertEqual({<<"rest">>, -1}, decode:decode_type(Input1, fixed_bitset)),
    Input2 = <<16, 1, 0, "rest">>,
    ?assertEqual({<<"rest">>, 256}, decode:decode_type(Input2, fixed_bitset)).

optional_test() ->
    ?assertEqual({<<"rest">>, {some, 42}}, decode:decode_type(<<42:32/signed-integer, "rest">>, {optional, int, true})),
    ?assertEqual({<<42:32/signed-integer, "rest">>, none}, decode:decode_type(<<42:32/signed-integer, "rest">>, {optional, int, false})),
    ?assertEqual({<<"rest">>, {some, "hello"}}, decode:decode_type(<<5, "hello", "rest">>, {optional, string, true})),
    ?assertEqual({<<"rest">>, none}, decode:decode_type(<<"rest">>, {optional, string, false})).

prefixed_optional_test() ->
    ?assertEqual({<<"rest">>, {some, 42}}, decode:decode_type(<<1, 42:32/signed-integer, "rest">>, {optional, int})),
    ?assertEqual({<<"rest">>, none}, decode:decode_type(<<0, "rest">>, {optional, int})).


array_test() ->
    ?assertEqual({<<"rest">>, [true, false]}, decode:decode_type(<<1, 0, "rest">>, {array, 2, bool})),
    ?assertEqual({<<"rest">>, [1, 2]}, decode:decode_type(<<1:32/signed-integer, 2:32/signed-integer, "rest">>, {array, int, 2})).

prefixed_array_test() ->
    ?assertEqual({<<"rest">>, [10, 20]}, decode:decode_type(<<2, 10, 20, "rest">>, {prefixed_array, byte})),
    ?assertEqual({<<"rest">>, []}, decode:decode_type(<<0, "rest">>, {prefixed_array, byte})),
    ?assertEqual({<<"rest">>, [100, 200]}, decode:decode_type(<<2, 100:32/signed-integer, 200:32/signed-integer, "rest">>, {prefixed_array, int})),
    ?assertEqual({<<"rest">>, [10, 20]}, decode:decode_type(<<0, 2, 10, 20, "rest">>, {prefixed_array, short, byte})).

enum_test() ->
    ?assertEqual({<<"rest">>, 42}, decode:decode_type(<<42, "rest">>, enum)),
    ?assertEqual({<<"rest">>, 42}, decode:decode_type(<<42, "rest">>, {enum, varint})),
    ?assertEqual({<<"rest">>, 42}, decode:decode_type(<<42, "rest">>, {enum, byte})),
    ?assertEqual({<<"rest">>, 42}, decode:decode_type(<<42:32/signed-integer, "rest">>, {enum, int})),
    ?assertEqual({<<"rest">>, south}, decode:decode_type(<<1, "rest">>, {enum, [north, south, east, west]})),
    ?assertEqual({<<"rest">>, east}, decode:decode_type(<<2, "rest">>, {enum, byte, [north, south, east, west]})),
    ?assertEqual({error, "invalid enum value"}, decode:decode_type(<<99, "rest">>, {enum, [north, south, east, west]})).

byte_array_test() ->
    ?assertEqual({<<>>, <<"hello">>}, decode:decode_type(<<"hello">>, byte_array)),
    ?assertEqual({<<"world">>, <<"hello">>}, decode:decode_type(<<"helloworld">>, {byte_array, 5})),
    ?assertEqual({<<"world">>, <<"hello">>}, decode:decode_type(<<5, "helloworld">>, {byte_array, varint})),
    ?assertEqual({<<"world">>, <<"hello">>}, decode:decode_type(<<0, 5, "helloworld">>, {byte_array, short})).

slot_test() ->
    %% Empty slot
    ?assertEqual({<<>>, empty}, decode:decode_type(<<0>>, slot)),
    ?assertEqual({<<"rest">>, empty}, decode:decode_type(<<0, "rest">>, slot)),
    %% Non-empty, no components
    ?assertEqual({<<>>, {1, 5, [], []}},
                 decode:decode_type(<<1, 5, 0, 0>>, slot)),
    %% With one add {TypeId=3, Data=<<1,2>>} and one remove TypeId=7
    Input = <<2, 10, 1, 1, 3, 2, 1, 2, 7>>,
    ?assertEqual({<<>>, {2, 10, [{3, <<1, 2>>}], [7]}},
                 decode:decode_type(Input, slot)).

hashed_slot_test() ->
    %% Empty hashed slot
    ?assertEqual({<<>>, empty}, decode:decode_type(<<0>>, hashed_slot)),
    ?assertEqual({<<"rest">>, empty}, decode:decode_type(<<0, "rest">>, hashed_slot)),
    %% Non-empty, no components
    ?assertEqual({<<>>, {5, 1, [], []}},
                 decode:decode_type(<<1, 5, 1, 0, 0>>, hashed_slot)),
    %% With one add {TypeId=3, Hash=12345} and one remove TypeId=7
    Input = <<1, 5, 1, 1, 3, 12345:32/signed-integer, 1, 7>>,
    ?assertEqual({<<>>, {5, 1, [{3, 12345}], [7]}},
                 decode:decode_type(Input, hashed_slot)).
