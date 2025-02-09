-module(data_packets).

-export([get_handshake_packet_name/1, 
        get_messages_serverbound/1,
        get_status_packet_name_clientbound/1,
        get_status_packet_name_serverbound/1,
        get_login_packet_name_clientbound/1,
        get_login_packet_name_serverbound/1,
        get_messages_clientbound/1,
        get_packet_number_clientbound/1]).

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
            cookie_response;
        2 ->
            custom_payload;
        3 ->
            finish_configuration;
        4 ->
            keep_alive;
        5 ->
            pong;
        6 ->
            resource_pack;
        7 ->
            select_known_packs
    end.


get_configuration_packet_name_clientbound(ID) ->
    case ID of
        0 ->
            cookie_request;
        1 ->
            clientbound_plugin_message;
        2 ->
            disconnect_configuration;
        3 ->
            finish_configuration;
        4 ->
            keep_alive;
        5 ->
            ping;
        6 ->
            reset_chat;
        7 ->
            registry_data;
        8 ->
            resource_pack_pop;
        9 ->
            resource_pack_push;
        10 ->
            store_cookie;
        11 ->
            transfer;
        12 ->
            update_enabled_features;
        13 ->
            update_tags;
        14 ->
            select_known_packs;
        15 ->
            custom_report_details;
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
        cookie_request_login -> 5
    end.

get_messages_serverbound(Id) ->
    case Id of
        handshake ->
            {handshake, [varint, string, ushort, varint]};
        login_start ->
            {login_start, [string, uuid]};
        encryption_response ->
            {encryption_response, [byte_array, byte_array]};
        login_plugin_response ->
            {login_plugin_response, [varint, byte_array]};
        login_acknowledged ->
            {login_acknowledged, []};
        cookie_reponse_login ->
            {cookie_reponse_login, [identifier, prefixed_array]};
        client_information ->
            {client_information, [string, byte, varint, bool, ubyte, varint, bool, bool, varint]};
        cookie_response ->
            {cookie_response, [identifier]}
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
            {cookie_request_login, [identifier]}
    end.
        
