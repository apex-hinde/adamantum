-module(decode_tests).
-include_lib("eunit/include/eunit.hrl").

decode_test() ->
    ?assertEqual(decode:decode_message(convert_message_for_decode(<<16,0,129,6,9,108,111,99,97,108,104,111,115,116,99,221,2>>), handshake), [769,"localhost",25565,2]),
    ?assertEqual(decode:decode_message(convert_message_for_decode(<<32,0,14,70,108,97,109,101,98,117,114,115,116,49,50,49,50,98,194,6,39,86,225,64,90,149,50,81,171,52,179,254,125>>), login_start), ["Flameburst1212",<<98,194,6,39,86,225,64,90,149,50,81,171,52,179,254,125>>]).



convert_message_for_decode(Data) ->
    {_, Data2} = varint:decode_varint(Data),
    {_, Data3} = varint:decode_varint(Data2),
    Data3.
