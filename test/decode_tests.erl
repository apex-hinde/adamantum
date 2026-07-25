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
    ?assertEqual({error, "insufficient data"}, decode:decode_type(<<128>>, varint)).

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

