-module(encode_messages).
-include("src/data_types/records.hrl").
-export([encode_message/2]).


encode_message(PacketName, Record) ->
    case PacketName of
        'minecraft:status_response' ->
            encode_status_response(Record);
        'minecraft:pong_response' -> 
            encode_pong_request(Record);
        'minecraft:login_finished' -> 
            encode_login_finished(Record)
    end.

encode_status_response(Record) ->
    {'minecraft:status_response', [Record#'minecraft:status_response'.json_response]}.

encode_pong_request(Record) ->
    {'minecraft:pong_response', [Record#'minecraft:pong_response'.timestamp]}.

encode_login_finished(Record) ->
    {'minecraft:login_finished', [Record#'minecraft:login_finished'.profile, Record#'minecraft:login_finished'.session_id]}.