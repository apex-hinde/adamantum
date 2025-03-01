-module(data_packets).

-export([get_handshake_packet_name/1, 
        get_messages_serverbound/1,
        get_status_packet_name_clientbound/1,
        get_status_packet_name_serverbound/1,
        get_login_packet_name_clientbound/1,
        get_login_packet_name_serverbound/1,
        get_messages_clientbound/1,
        get_packet_number_clientbound/1,
        get_configuration_packet_name_clientbound/1,
        get_configuration_packet_name_serverbound/1]).

get_handshake_packet_name(_ID) ->
    handshake.

get_status_packet_name_serverbound(ID) ->
    case ID of
        0 ->
            status_request;
        1 ->
            ping_request
    end.

get_status_packet_name_clientbound(ID) ->
    case ID of
        0 ->
            status_request;
        1 ->
            ping_request
    end.

get_login_packet_name_serverbound(ID) ->
    case ID of
        0 ->
            login_start;
        1 ->
            encryption_response;
        2 ->
            login_plugin_response;
        3 ->
            login_acknowledged;
        4 ->
            cookie_reponse_login
    end.
get_login_packet_name_clientbound(ID) ->
    case ID of
        0 ->
            login_disconnect;
        1 ->
            encryption_request;
        2 ->
            login_success;
        3 ->
            set_compression;
        4 ->
            login_plugin_request;
        5 ->
            cookie_request
    end.

get_configuration_packet_name_serverbound(ID) ->
    case ID of
        0 ->
            client_information;
        1 ->
            cookie_response_configuration;
        2 ->
            serverbound_plugin_message_configuration;
        3 ->
            acknowledge_finish_configuration;
        4 ->
            serverbound_keep_alive_configuration;
        5 ->
            pong_configuration;
        6 ->
            resource_pack_responce_configuration;
        7 ->
            serverbound_known_packs
    end.


get_configuration_packet_name_clientbound(ID) ->
    case ID of
        0 ->
            cookie_request_configuration;
        1 ->
            clientbound_plugin_message_configuration;
        2 ->
            disconnect_configuration;
        3 ->
            finish_configuration;
        4 ->
            clientbound_keep_alive_configuration;
        5 ->
            ping_configuration;
        6 ->
            reset_chat;
        7 ->
            registry_data;
        8 ->
            remove_resource_pack_configuration;
        9 ->
            add_resource_pack_configuration;
        10 ->
            store_cookie_configuration;
        11 ->
            transfer_configuration;
        12 ->
            feature_flags;
        13 ->
            update_tags_configuration;
        14 ->
            clientbound_known_packs;
        15 ->
            custom_report_details_configuration;
        16 ->
            server_links
    end.
        
        

get_packet_number_clientbound(ID) ->
    case ID of
        login_disconnect -> 0;
        encryption_request -> 1;
        login_success -> 2;
        set_compression -> 3;
        login_plugin_request -> 4;
        cookie_request_login -> 5;
        cookie_request_configuration -> 0;
        clientbound_plugin_message_configuration -> 1;
        disconnect_configuration -> 2;
        finish_configuration -> 3;
        clientbound_keep_alive_configuration -> 4;
        ping_configuration -> 5;
        reset_chat -> 6;
        registry_data -> 7;
        remove_resource_pack_configuration -> 8;
        add_resource_pack_configuration -> 9;
        store_cookie_configuration -> 10;
        transfer_configuration -> 11;
        feature_flags -> 12;
        update_tags_configuration -> 13;
        clientbound_known_packs -> 14;
        custom_report_details_configuration -> 15;
        server_links -> 16
    end.

get_messages_serverbound(Id) ->
    case Id of
        handshake ->
            {handshake, [varint, string, ushort, varint]};
        login_start ->
            {login_start, [string, uuid]};
        encryption_response ->
            {encryption_response, [{prefixed_array, [byte]} , {prefixed_array, [byte]}]};
        login_plugin_response ->
            {login_plugin_response, [varint, {prefixed_optional, byte_array_end}]};
        login_acknowledged ->
            {login_acknowledged, []};
        cookie_reponse_login ->
            {cookie_reponse_login, [identifier, {prefixed_array, [byte]}]};
        client_information ->
            {client_information, [string, byte, varint, bool, ubyte, varint, bool, bool, varint]};
        cookie_response_configuration ->
            {cookie_response, [identifier, {{prefixed_array, [byte]}, {prefixed_array, [byte]}}]};
        serverbound_plugin_message_configuration ->
            {plugin_message_configuration, [identifier, byte_array_end]};
        acknowledge_finish_configuration ->
            {acknowledge_finish_configuration, []};
        serverbound_keep_alive_configuration ->
            {keep_alive_configuration, [long]};
        pong_configuration ->
            {pong_configuration, [int]};
        resource_pack_responce_configuration ->
            {resource_pack_responce_configuration, [uuid, varint]};
        serverbound_known_packs ->
            {serverbound_known_packs, [string, string, string]}

    end.


get_messages_clientbound(Id) ->
    case Id of
        login_disconnect ->
            {login_disconnect, [json_text]};
        encryption_request ->
            {encryption_request, [string, prefixed_array, prefixed_array, bool]};
        login_success ->
            {login_success, [uuid, string, prefixed_array_login]};
        set_compression ->
            {set_compression, [varint]};
        login_plugin_request ->
            {login_plugin_request, [varint, identifier, byte_array]};
        cookie_request_login ->
            {cookie_request_login, [identifier]};
        cookie_request_configuration ->
            {cookie_request_configuration, [identifier]};
        clientbound_plugin_message_configuration ->
            {plugin_message_configuration, [identifier, byte_array_end]};
        disconnect_configuration ->
            {disconnect_configuration, [text_component]};
        finish_configuration ->
            {finish_configuration, []};
        clientbound_keep_alive_configuration ->
            {keep_alive_configuration, [long]};
        ping_configuration ->
            {ping_configuration, [int]};
        reset_chat ->
            {reset_chat, []};
        registry_data ->
            {registry_data, [identifier, {prefixed_array, [identifier, {prefixed_optional, nbt}]}]};
        remove_resource_pack_configuration ->
            {remove_resource_pack_configuration, [{prefixed_optional, uuid}]};
        add_resource_pack_configuration ->
            {add_resource_pack_configuration, [uuid, string, string, bool, {prefixed_optional, text_component}]};
        store_cookie_configuration ->
            {store_cookie_configuration, [identifier, {prefixed_array, [byte]}]};
        transfer_configuration ->
            {transfer_configuration, [string, varint]};
        feature_flags ->
            {feature_flags, [{prefixed_array, [identifier]}]};
        update_tags_configuration ->
            {update_tags_configuration, [{prefixed_array, [identifier, {prefixed_array, [identifier, varint]}]}]};
        clientbound_known_packs ->
            {clientbound_known_packs, [{prefixed_array, [string, string, string]}]};
        custom_report_details_configuration ->
            {custom_report_details_configuration, [{prefixed_array, [string, string]}]};
        server_links ->
            {server_links, [{prefixed_array, [bool, varint, string]}]}

            

    end.
        
