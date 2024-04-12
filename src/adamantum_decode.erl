-module(adamantum_decode).
-export([decode_handshake/1]).

decode_handshake(Data) ->
    Next_state = binary:last(Data),
    case Next_state of
        1 ->
            State = status,
            State;
        2 ->
            State = login,
            State
        end.