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
            server_links;
        39 ->
            clientbound_keep_alive_play
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

get_play_packet_name_clientbound(ID) ->
    case ID of 
        0 ->
            bundle_delimiter;
        1 ->
            spawn_entity;
        2 ->
            spawn_experience_orb;
        3 ->
            entity_animation;
        4 ->
            award_statistics;
        5 ->
            acknowledge_block_change;
        6 ->
            set_block_destroy_change;
        7 ->
            block_entity_data;
        8 ->
            block_action;
        9 ->
            block_update;
        10 ->
            boss_bar;
        11 ->
            change_difficulty;
        12 ->
            chunk_batch_finished;
        13 ->
            chunk_batch_start;
        14 ->
            chunks_biomes;
        15 ->
            clear_titles;
        16 ->
            command_suggestions_responce;
        17 ->
            commands;
        18 ->
            close_container;
        19 ->
            set_container_content;
        20 ->
            set_container_property;
        21 ->
            set_container_slot;
        22 ->
            cookie_request_play;
        23 ->
            set_cooldown;
        24 ->
            chat_suggestions;
        25 ->
            clientbound_plugin_message_play;
        26 ->
            damage_event;
        27 ->
            debug_sample;
        28 ->
            delete_message;
        29 ->
            disconnect_play;
        30 ->
            disguised_chat_message;
        31 ->
            entity_event;
        32 ->
            teleport_entity;
        33 ->
            explosion;
        34 ->
            unload_chunk;
        35 ->
            game_event;
        36 ->
            open_horse_screen;
        37 ->
            hurt_animation;
        38 ->
            initialize_world_border;
        39 ->
            clientbound_keep_alive_play;
        40 ->
            chunk_data_and_update_light;
        41 ->
            world_event;
        42 ->
            particle;
        43 ->
            update_light;
        44 ->
            login_play;
        45 ->
            map_data;
        46 ->
            merchant_offers;
        47 ->
            update_entity_position;
        48 ->
            update_entity_position_and_rotation;
        49 ->
            move_minecart_along_track;
        50 ->
            update_entity_rotation;
        51 ->
            move_vehicle;
        52 ->
            open_book;
        53 ->
            open_screen;
        54 ->
            open_sign_editor;
        55 ->
            ping_play;
        56 ->
            ping_response_play;
        57 ->
            place_ghost_recipe;
        58 ->
            player_abilities_clientbound;
        59 ->
            player_chat_message;
        60 ->
            end_combat;
        61 ->
            enter_combat;
        62 ->
            combat_death;
        63 ->
            player_info_remove;
        64 ->
            player_info_update;
        65 ->
            look_at;
        66 ->
            syncronize_player_position;
        67 ->
            player_rotation;
        68 ->
            recipe_book_add;
        69 ->
            recipe_book_remove;
        70 ->
            recipe_book_settings;
        71 ->
            remove_entities;
        72 ->
            remove_entity_effect;
        73 ->
            reset_score;
        74 ->
            remove_resource_pack_play;
        75 ->
            add_resource_pack_play;
        76 ->
            respawn;
        77 ->
            set_head_rotation;
        78 ->
            update_section_blocks;
        79 ->
            select_advancements_tab;
        80 ->
            server_data;
        81 ->
            set_action_bar_text;
        82 ->
            set_border_center;
        83 ->
            set_border_lerp_size;
        84 ->
            set_border_size;
        85 ->
            set_border_warning_delay;
        86 ->
            set_border_warning_distance;
        87 ->
            set_camera;
        88 ->
            set_centre_chunk;
        89 ->
            set_render_distance;
        90 ->
            set_cursor_item;
        91 ->
            set_default_spawn_position;
        92 ->
            display_objective;
        93 ->
            set_entity_metadata;
        94 ->
            link_entities;
        95 ->
            set_entity_velocity;
        96 ->
            set_equipment;
        97 ->
            set_experience;
        98 ->
            set_health;
        99 ->
            set_held_item_clientbound;
        100 ->
            update_objective;
        101 ->
            set_passengers;
        102 ->
            set_player_inventory_slot;
        103 ->
            update_teams;
        104 ->
            update_score;
        105 ->
            set_simulation_distance;
        106 ->
            set_subtitle_text;
        107 ->
            update_time;
        108->
            set_title_text;
        109 ->
            set_title_animation_time;
        110 ->
            entity_sounds_effect;
        111 ->
            sound_effect;
        112 ->
            start_configuration;
        113 ->
            stop_sound;
        114 ->
            store_cookie_play;
        115 ->
            system_chat_message;
        116 ->
            set_tab_list_header_and_footer;
        117 ->
            tag_query_response;
        118 ->
            pickup_item;
        119 ->
            syncronize_vehicle_position;
        120 ->
            set_ticking_state;
        121 ->
            step_tick;
        122 ->
            transfer_play;
        123 ->
            update_advancements;
        124 ->
            update_attributes;
        125 ->
            entity_effect;
        126 ->
            update_recipies;
        127 ->
            update_tags_play;
        128 ->
            projectile_power;
        129 ->
            custom_report_details;
        130 ->
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
        server_links -> 16;
        bundle_delimiter -> 0;
        spawn_entity -> 1;
        spawn_experience_orb -> 2;
        entity_animation -> 3;
        award_statistics -> 4;
        acknowledge_block_change -> 5;
        set_block_destroy_change -> 6;
        block_entity_data -> 7;
        block_action -> 8;
        block_update -> 9;
        boss_bar -> 10;
        change_difficulty -> 11;
        chunk_batch_finished -> 12;
        chunk_batch_start -> 13;
        chunks_biomes -> 14;
        clear_titles -> 15;
        command_suggestions_responce -> 16;
        commands -> 17;
        close_container -> 18;
        set_container_content -> 19;
        set_container_property -> 20;
        set_container_slot -> 21;
        cookie_request_play -> 22;
        set_cooldown -> 23;
        chat_suggestions -> 24;
        clientbound_plugin_message_play -> 25;
        damage_event -> 26;
        debug_sample -> 27;
        delete_message -> 28;
        disconnect_play -> 29;
        disguised_chat_message -> 30;
        entity_event -> 31;
        teleport_entity -> 32;
        explosion -> 33;
        unload_chunk -> 34;
        game_event -> 35;
        open_horse_screen -> 36;
        hurt_animation -> 37;
        initialize_world_border -> 38;
        clientbound_keep_alive_play -> 39;
        chunk_data_and_update_light -> 40;
        world_event -> 41;
        particle -> 42;
        update_light -> 43;
        login_play -> 44;
        map_data -> 45;
        merchant_offers -> 46;
        update_entity_position -> 47;
        update_entity_position_and_rotation -> 48;
        move_minecart_along_track -> 49;
        update_entity_rotation -> 50;
        move_vehicle -> 51;
        open_book -> 52;
        open_screen -> 53;
        open_sign_editor -> 54;
        ping_play -> 55;
        ping_response_play -> 56;
        place_ghost_recipe -> 57;
        player_abilities_clientbound -> 58;
        player_chat_message -> 59;
        end_combat -> 60;
        enter_combat -> 61;
        combat_death -> 62;
        player_info_remove -> 63;
        player_info_update -> 64;
        look_at -> 65;
        syncronize_player_position -> 66;
        player_rotation -> 67;
        recipe_book_add -> 68;
        recipe_book_remove -> 69;
        recipe_book_settings -> 70;
        remove_entities -> 71;
        remove_entity_effect -> 72;
        reset_score -> 73;
        remove_resource_pack_play -> 74;
        add_resource_pack_play -> 75;
        respawn -> 76;
        set_head_rotation -> 77;
        update_section_blocks -> 78;
        select_advancements_tab -> 79;
        server_data -> 80;
        set_action_bar_text -> 81;
        set_border_center -> 82;
        set_border_lerp_size -> 83;
        set_border_size -> 84;
        set_border_warning_delay -> 85;
        set_border_warning_distance -> 86;
        set_camera -> 87;
        set_centre_chunk -> 88;
        set_render_distance -> 89;
        set_cursor_item -> 90;
        set_default_spawn_position -> 91;
        display_objective -> 92;
        set_entity_metadata -> 93;
        link_entities -> 94;
        set_entity_velocity -> 95;
        set_equipment -> 96;
        set_experience -> 97;
        set_health -> 98;
        set_held_item_clientbound -> 99;
        update_objective -> 100;
        set_passengers -> 101;
        set_player_inventory_slot -> 102;
        update_teams -> 103;
        update_score -> 104;
        set_simulation_distance -> 105;
        set_subtitle_text -> 106;
        update_time -> 107;
        set_title_text -> 108;
        set_title_animation_time -> 109;
        entity_sounds_effect -> 110;
        sound_effect -> 111;
        start_configuration -> 112;
        stop_sound -> 113;
        store_cookie_play -> 114;
        system_chat_message -> 115;
        set_tab_list_header_and_footer -> 116;
        tag_query_response -> 117;
        pickup_item -> 118;
        syncronize_vehicle_position -> 119;
        set_ticking_state -> 120;
        step_tick -> 121;
        transfer_play -> 122;
        update_advancements -> 123;
        update_attributes -> 124;
        entity_effect -> 125;
        update_recipies -> 126;
        update_tags_play -> 127;
        projectile_power -> 128;
        custom_report_details -> 129;
        server_links -> 130
        
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
            {edit_book, [varint, {prefixed_array, [string]}, {prefixed_optional, string}]};
        query_entity_tag ->
            {query_entity_tag, [varint, varint]};
        interact ->
            {interact, [varint, interact, bool]};
        jigsaw_generate ->
            {jigsaw_generate, [position, varint, bool]};
        serverbound_keep_alive_play ->
            {keep_alive_play, [long]};
        lock_dificulty ->
            {lock_dificulty, [bool]};
        set_player_position ->
            {set_player_position, [double, double, double, byte]};
        set_player_position_and_rotation ->
            {set_player_position_and_rotation, [double, double, double, float, float, byte]};
        set_player_rotation ->
            {set_player_rotation, [float, float, byte]};
        set_player_movement_flags ->
            {set_player_movement_flags, [byte]};
        move_vehicle ->
            {move_vehicle, [double, double, double, float, float, byte]};
        paddle_boat ->
            {paddle_boat, [bool, bool]};
        pick_item_from_block ->
            {pick_item_from_block, [position, bool]};
        pick_item_from_entity ->
            {pick_item_from_entity, [varint, bool]};
        ping_request_play ->
            {ping_request_play, [long]};
        place_recipe ->
            {place_recipe, [varint, varint, bool]};
        player_abilities_serverbound ->
            {player_abilities_serverbound, [byte]};
        player_action ->
            {player_action, [varint, position, byte, varint]};
        player_command ->
            {player_command, [varint, varint, varint]};
        player_input ->
            {player_input, [ubyte]};
        player_loaded ->
            {player_loaded, []};
        pong_play ->
            {pong_play, [int]};
        change_recipe_book_settings ->
            {change_recipe_book_settings, [varint, bool, bool]};
        set_seen_recipe ->
            {set_seen_recipe, [varint]};
        rename_item ->
            {rename_item, [string]};
        resource_pack_responce_play ->
            {resource_pack_responce_play, [uuid, varint]};
        seen_advancements ->
            {seen_advancements, [bool, {optional, identifier}]};
        select_trade ->
            {select_trade, [varint]};
        set_beacon_effect ->
            {set_beacon_effect, [{prefixed_optional, varint}, {prefixed_optional, varint}]};
        set_held_item_serverbound ->
            {set_held_item_serverbound, [short]};
        program_command_block ->
            {program_command_block, [position, string, varint, byte]};
        program_command_block_minecart ->
            {program_command_block_minecart, [varint, string, bool]};
        set_creative_mode_slot ->
            {set_creative_mode_slot, [short, slot]};
        program_jigsaw_block ->
            {program_jigsaw_block, [position, identifier, identifier, identifier, string, string, varint, varint]};
        program_structure_block ->
            {program_structure_block, [position, varint, varint, string, byte, byte, byte, byte, byte, byte, varint, varint, string, float, varlong, byte]};
        update_sign ->
            {update_sign, [position, bool, string, string, string, string]};
        swing_arm ->
            {swing_arm, [varint]};
        teleport_to_entity ->
            {teleport_to_entity, [uuid]};
        use_item_on ->
            {use_item_on, [varint, position, varint, float, float, float, bool, bool, varint]};
        use_item ->
            {use_item, [varint, varint, float, float]}
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
            {clientbound_keep_alive_configuration, [long]};
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
            {server_links, [{prefixed_array, [bool, varint, string]}]};
        bundle_delimiter -> 
            {bundle_delimiter, []};
        spawn_entity -> 
            {spawn_entity, [varint, uuid, varint, double, double, double, andgle, angle, angle, varint, short, short, short]};
        spawn_experience_orb -> 
            {spawn_experience_orb, [varint, double, double, double, short]};
        entity_animation -> 
            {entity_animation, [varint, ubyte]};
        award_statistics -> 
            {award_statistics, [{prefixed_array, [varint, varint, varint]}]};
        acknowledge_block_change -> 
            {acknowledge_block_change, [varint]};
        set_block_destroy_change -> 
            {set_block_destroy_change, [varint, position, ubyte]};
        block_entity_data -> 
            {block_entity_data, [position, varint, nbt]};
        block_action -> 
            {block_action, [position, ubyte, ubyte, varint]};
        block_update -> 
            {block_update, [position, varint]};
        boss_bar -> 
            {boss_bar, [uuid, boss_bar]};
        change_difficulty -> 
            {change_difficulty, [ubyte, bool]};
        chunk_batch_finished -> 
            {chunk_batch_finished, [varint]};
        chunk_batch_start -> 
            {chunk_batch_start, []};
        chunks_biomes -> 
            {chunks_biomes, [{prefixed_array, [int, int, {prefixed_array, [byte]}]}]};
        clear_titles -> 
            {clear_titles, [bool]};
        command_suggestions_responce -> 
            {command_suggestions_responce, [varint, varint, varint, {prefixed_array, [string, {prefixed_optional, text_component}]}]};
        commands -> 
            {commands, [{prefixed_array, [node]}, varint]};
        close_container -> 
            {close_container, [varint]};
        set_container_content -> 
            {set_container_content, [varint, varint, {prefixed_array, [slot]}, slot]};
        set_container_property -> 
            {set_container_property, [varint, short, short]};
        set_container_slot -> 
            {set_container_slot, [varint, varint, short, slot]};
        cookie_request_play -> 
            {cookie_request_play, [identifier]};
        set_cooldown -> 
            {set_cooldown, [varint, varint]};
        chat_suggestions -> 
            {chat_suggestions, [varint, {prefixed_array, [string]}]};
        clientbound_plugin_message_play -> 
            {plugin_message_play, [identifier, byte_array_end]};
        damage_event -> 
            {damage_event, [varint, varint, varint, bool, double, double, double]};
        debug_sample -> 
            {debug_sample, [{prefixed_array, [long]}, varint]};
        delete_message -> 
            {delete_message, [varint, byte_array]};
        disconnect_play -> 29;
        disguised_chat_message -> 30;
        entity_event -> 31;
        teleport_entity -> 32;
        explosion -> 33;
        unload_chunk -> 34;
        game_event -> 35;
        open_horse_screen -> 36;
        hurt_animation -> 37;
        initialize_world_border -> 38;
        clientbound_keep_alive_play -> 
            {clientbound_keep_alive_play, [long]};
        chunk_data_and_update_light -> 40;
        world_event -> 41;
        particle -> 42;
        update_light -> 43;
        login_play -> 44;
        map_data -> 45;
        merchant_offers -> 46;
        update_entity_position -> 47;
        update_entity_position_and_rotation -> 48;
        move_minecart_along_track -> 49;
        update_entity_rotation -> 50;
        move_vehicle -> 51;
        open_book -> 52;
        open_screen -> 53;
        open_sign_editor -> 54;
        ping_play -> 55;
        ping_response_play -> 56;
        place_ghost_recipe -> 57;
        player_abilities_clientbound -> 58;
        player_chat_message -> 59;
        end_combat -> 60;
        enter_combat -> 61;
        combat_death -> 62;
        player_info_remove -> 63;
        player_info_update -> 64;
        look_at -> 65;
        syncronize_player_position -> 66;
        player_rotation -> 67;
        recipe_book_add -> 68;
        recipe_book_remove -> 69;
        recipe_book_settings -> 70;
        remove_entities -> 71;
        remove_entity_effect -> 72;
        reset_score -> 73;
        remove_resource_pack_play -> 74;
        add_resource_pack_play -> 75;
        respawn -> 76;
        set_head_rotation -> 77;
        update_section_blocks -> 78;
        select_advancements_tab -> 79;
        server_data -> 80;
        set_action_bar_text -> 81;
        set_border_center -> 82;
        set_border_lerp_size -> 83;
        set_border_size -> 84;
        set_border_warning_delay -> 85;
        set_border_warning_distance -> 86;
        set_camera -> 87;
        set_centre_chunk -> 88;
        set_render_distance -> 89;
        set_cursor_item -> 90;
        set_default_spawn_position -> 91;
        display_objective -> 92;
        set_entity_metadata -> 93;
        link_entities -> 94;
        set_entity_velocity -> 95;
        set_equipment -> 96;
        set_experience -> 97;
        set_health -> 98;
        set_held_item_clientbound -> 99;
        update_objective -> 100;
        set_passengers -> 101;
        set_player_inventory_slot -> 102;
        update_teams -> 103;
        update_score -> 104;
        set_simulation_distance -> 105;
        set_subtitle_text -> 106;
        update_time -> 107;
        set_title_text -> 108;
        set_title_animation_time -> 109;
        entity_sounds_effect -> 110;
        sound_effect -> 111;
        start_configuration -> 112;
        stop_sound -> 113;
        store_cookie_play -> 114;
        system_chat_message -> 115;
        set_tab_list_header_and_footer -> 116;
        tag_query_response -> 117;
        pickup_item -> 118;
        syncronize_vehicle_position -> 119;
        set_ticking_state -> 120;
        step_tick -> 121;
        transfer_play -> 122;
        update_advancements -> 123;
        update_attributes -> 124;
        entity_effect -> 125;
        update_recipies -> 126;
        update_tags_play -> 127;
        projectile_power -> 128;
        custom_report_details -> 129;
        server_links -> 130

    end.
        
