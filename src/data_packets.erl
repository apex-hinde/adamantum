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
        get_configuration_packet_name_serverbound/1,
        get_play_packet_name_serverbound/1]).

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
        
get_play_packet_name_serverbound(ID) ->
    case ID of
        0 ->
            confirm_teleportation;
        1 ->
            query_block_entity_tag;
        2 ->
            bundle_item_selected;
        3 ->
            change_difficulty;
        4 ->
            acknowledge_message;
        5 ->
            chat_command;
        6 ->
            signed_chat_command;
        7 ->
            chat_message;
        8 ->
            player_session;
        9 ->
            chunk_batch_recieved;
        10 ->
            client_status;
        11 ->
            client_tick_end;
        12 ->
            client_information_play;
        13 ->
            command_suggestions_request;
        14 ->
            acknowledge_configuration;
        15 ->
            click_container_button;
        16 ->
            click_container;
        17 ->
            close_container;
        18 ->
            change_container_slot_state;
        19 ->
            cookie_response_play;
        20 ->
            serverbound_plugin_message_play;
        21 ->
            debug_sample_subscription;
        22 ->
            edit_book;
        23 ->
            quert_entity_tag;
        24 ->
            interact;
        25 ->
            jigsaw_generate;
        26 ->
            serverbound_keep_alive_play;
        27 ->
            lock_dificulty;
        28 ->
            set_player_position;
        29 ->
            set_player_position_and_rotation;
        30 ->
            set_player_rotation;
        31 ->
            set_player_movement_flags;
        32 ->
            move_vehicle;
        33 ->
            paddle_boat;
        34 ->
            pick_item_from_block;
        35 ->
            pick_item_from_entity;
        36 ->
            ping_request_play;
        37 ->
            place_recipe;
        38 ->
            player_abilities_serverbound;
        39 ->
            player_action;
        40 ->
            player_command;
        41 ->
            player_input;
        42 ->
            player_loaded;
        43 ->
            pong_play;
        44 ->
            change_recipe_book_settings;
        45 ->
            set_seen_recipe;
        46 ->
            rename_item;
        47 ->
            resource_pack_responce_play;
        48 ->
            seen_advancements;
        49 ->
            select_trade;
        50 ->
            set_beacon_effect;
        51 ->
            set_held_item_serverbound;
        52 ->
            program_command_block;
        53 ->
            program_command_block_minecart;
        54 ->
            set_creative_mode_slot;
        55 ->
            program_jigsaw_block;
        56 ->
            program_structure_block;
        57 ->
            update_sign;
        58 ->
            swing_arm;
        59 ->
            teleport_to_entity;
        60 ->
            use_item_on;
        61 ->
            use_item

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
            {serverbound_known_packs, [string, string, string]};
        confirm_teleportation ->
            {confirm_teleportation, [varint]};
        query_block_entity_tag ->
            {query_block_entity_tag, [varint, position]};
        bundle_item_selected ->
            {bundle_item_selected, [varint, varint]};
        change_difficulty ->
            {change_difficulty, [ubyte]};
        acknowledge_message ->
            {acknowledge_message, [varint]};
        chat_command ->
            {chat_command, [string]};
        signed_chat_command ->
            {signed_chat_command, [string, long, long, {prefixed_array, [string, byte_array_chat]}, varint, fixed_bitset]};
        chat_message ->
            {chat_message, [string, lomg, long, {prefixed_optional, byte_array_chat, varint, fixed_bitset}]};
        player_session ->
            {player_session, [uuid, long, {prefixed_array, [byte]}, {prefixed_array, [byte]}]};
        chunk_batch_recieved ->
            {chunk_batch_recieved, [float]};
        client_status ->
            {client_status, [varint]};
        client_tick_end ->
            {client_tick_end, []};
        client_information_play ->
            {client_information_play, [string, byte, varint, bool, ubyte, varint, bool, bool, varint]};
        command_suggestions_request ->
            {command_suggestions_request, [varint, string]};
        acknowledge_configuration ->
            {acknowledge_configuration, []};
        click_container_button ->
            {click_container_button, [varint, varint]};
        click_container ->
            {click_container, [varint, varint, short, byte, varint, {prefixed_array, [short, slot]}, slot]};
        close_container ->
            {close_container, [varint]};
        change_container_slot_state ->
            {change_container_slot_state, [varint, varint, byte]};
        cookie_response_play ->
            {cookie_response_play, [identifier, {prefixed_array, [byte]}]};
        serverbound_plugin_message_play ->
            {plugin_message_play, [identifier, byte_array_end]};
        debug_sample_subscription ->
            {debug_sample_subscription, [varint]};
        edit_book ->
            {edit_book, [varint, {prefixed_array, [string]}, {prefixed_optional, string}]}


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
        
