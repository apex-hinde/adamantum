-module(varint).
-export([decode_varint/1,encode_varint/1,encode_string/1,encode_binary/4]).

-spec decode_varint(binary()) -> {non_neg_integer(), binary()}.
decode_varint(Data) when is_binary(Data) ->
    decode_varint(Data, 0, 0).

decode_varint(<<1:1, Number:7, Rest/binary>>, Position, Acc) ->
    decode_varint(Rest, Position + 7, (Number bsl Position) + Acc);
decode_varint(<<0:1, Number:7, Rest/binary>>, Position, Acc) ->
    {(Number bsl Position) + Acc, Rest};
decode_varint(<<>>, _, _) ->
    {error, "insufficient data"}.

-spec encode_varint(non_neg_integer()) -> binary().
encode_varint(I) when is_integer(I), I >= 0, I =< 127 ->
    <<I>>;
encode_varint(I) when is_integer(I), I > 127 ->
    <<1:1, (I band 127):7, (encode_varint(I bsr 7))/binary>>;
encode_varint(I) when is_integer(I), I < 0 ->
    erlang:error({badarg, I}).


encode_string(Data) -> 
    list_to_binary(Data),
    Length = size(Data).


encode_binary(Length,Packet_id,Length_username,Username) ->



      Length1 = encode_varint(Length),
      Packet_id1 = encode_varint(Packet_id),
      Length_username1 = encode_varint(Length_username),
    
    <<Length1/binary, Packet_id1/binary,Length_username1/binary,Username/binary>>.





