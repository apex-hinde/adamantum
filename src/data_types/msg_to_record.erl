-module(msg_to_record).

-export([msg_to_record/1]).
-include("src/data_types/records.hrl").


msg_to_record(Msg) ->
    case Msg of
        {'minecraft:intention', [Protocol_version, Server_address, Server_port, Next_state]} ->
            Protocol_version2 = decode:extract_value(Protocol_version),
            Server_address2 = decode:extract_value(Server_address),
            Server_port2 = decode:extract_value(Server_port),
            Next_state2 = decode:extract_value(Next_state),
            #'minecraft:intention'{protocol_version = Protocol_version2, server_address = Server_address2, server_port = Server_port2, next_state = Next_state2};
        {'minecraft:ping_request', [Timestamp]} ->
            Timestamp2 = decode:extract_value(Timestamp),
            #'minecraft:pong_response'{timestamp = Timestamp2};
        {'minecraft:hello', [Name, UUID]} ->
            Name2 = decode:extract_value(Name),
            UUID2 = decode:extract_value(UUID),
            #'minecraft:hello_serverbound'{name = Name2, uuid = UUID2};
        {'minecraft:key', [Public_key, Nounce]} ->
            Public_key2 = decode:extract_value(Public_key),
            Nounce2 = decode:extract_value(Nounce),
            #'minecraft:key'{public_key = Public_key2, nonce = Nounce2};
        {'minecraft:login_acknowledged', []} ->
            #'minecraft:login_acknowledged'{};
        {'minecraft:cookie_response', [Cookie, Data]} ->
            Cookie2 = decode:extract_value(Cookie),
            Data2 = decode:extract_value(Data),
            #'minecraft:cookie_response'{cookie = Cookie2, data = Data2};
        {'minecraft:custom_payload', [Channel, Data]} ->
            Channel2 = decode:extract_value(Channel),
            Data2 = decode:extract_value(Data),
            #'minecraft:custom_payload'{channel = Channel2, data = Data2};
        {'minecraft:client_information', [Locale, View_distance, Chat_mode, Chat_colors, Displayed_skin_parts, Main_hand, Enable_text_filtering, Allow_server_listings, Particle_status]} ->
            Locale2 = decode:extract_value(Locale),
            View_distance2 = decode:extract_value(View_distance),
            Chat_mode2 = decode:extract_value(Chat_mode),
            Chat_colors2 = decode:extract_value(Chat_colors),
            Displayed_skin_parts2 = decode:extract_value(Displayed_skin_parts),
            Main_hand2 = decode:extract_value(Main_hand),
            Enable_text_filtering2 = decode:extract_value(Enable_text_filtering),
            Allow_server_listings2 = decode:extract_value(Allow_server_listings),
            Particle_status2 = decode:extract_value(Particle_status),
            #'minecraft:client_information'{
                locale = Locale2,
                view_distance = View_distance2,
                chat_mode = Chat_mode2,
                chat_colors = Chat_colors2,
                displayed_skin_parts = Displayed_skin_parts2,
                main_hand = Main_hand2,
                enable_text_filtering = Enable_text_filtering2,
                allow_server_listings = Allow_server_listings2,
                particle_status = Particle_status2
            };

        _ ->
            io:format("Error: Unknown message type: ~p~n", [Msg]),
            ok

    end.


