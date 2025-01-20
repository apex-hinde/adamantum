-module(data_packets).
-export([get_messages_clientbound/1, get_messages_serverbound/1, get_handshake_packet_name/1, get_login_packet_name_clientbound/1
        ,get_login_packet_name_serverbound/1, get_play_packet_name_clientbound/1, get_play_packet_name_serverbound/1
        ,get_packet_number_clientbound/1]).


    






get_messages_clientbound(Id) ->
    case Id of 
        disconnect ->
            {disconnect, [chat]};
        encryption_request ->
            {encryption_request, [string, varint, byte_array, varint, byte_array]};
        login_success ->
            {login_success, [string, string]};
        set_compression ->
            {set_compression, [varint]};
        keep_alive ->
            {keep_alive, [varint]};
        join_game ->
            {join_game, [int, ubyte, byte, ubyte, string, bool]};
        chat_message ->
            {chat_message, [chat, byte]};
        time_update ->
            {time_update, [long, long]};
        entity_equipment ->
            {entity_equipment, [varint, short, slot]};
        spawn_position ->
            {spawn_position, [position]};
        update_health ->
            {update_health, [float, varint, float]};
        respawn ->
            {respawn, [int, ubyte, ubyte, string]};
        player_position_and_look ->
            {player_position_and_look, [double, double, double, float, float, byte]};
        held_item_change ->
            {held_item_change, [slot]};
        use_bed ->
            {use_bed, [varint, position]};
        animation ->
            {animation, [varint, ubyte]};
        spawn_player ->
            {spawn_player, [varint, uuid, int, int, int, angle, angle, short, metadata]};
        collect_item ->
            {collect_item, [varint, varint]};
        spawn_object ->
            {spawn_object, [varint, byte, int, int, int, angle, angle, int, oshort, oshort, oshort]};
        spawn_mob ->
            {spawn_mob, [varint, ubyte, int, int, int, angle, angle, angle, short, short, short, metadata]};
        spawn_painting ->
            {spawn_painting, [varint, string, position, ubyte]};
        spawn_experience_orb ->
            {spawn_experience_orb, [varint, int, int, int, short]};
        entity_velocity ->
            {entity_velocity, [varint, short, short, short]};
        destroy_entities ->
            {destroy_entities, [varint, array_of_varint]}; %check later
        entity ->
            {entity, [varint]};
        entity_relative_move ->
            {entity_relative_move, [varint, byte, byte, byte, bool]};
        entity_look ->
            {entity_look, [varint, angle, angle, bool]};
        entity_look_and_relative_move ->
            {entity_look_and_relative_move, [varint, byte, byte, byte, angle, angle, bool]};
        entity_teleport ->
            {entity_teleport, [varint, int, int, int, angle, angle, bool]};
        entity_head_look ->
            {entity_head_look, [varint, angle]};
        entity_status ->
            {entity_status, [int, byte]};
        attach_entity ->
            {attach_entity, [int, int, bool]};
        entity_metadata ->
            {entity_metadata, [varint, metadata]};
        entity_effect ->
            {entity_effect, [varint, byte, byte, varint, bool]};
        remove_entity_effect ->
            {remove_entity_effect, [varint, byte]};
        set_experience ->
            {set_experience, [float, varint, varint]};
        entity_properties ->
            {entity_properties, [varint, int, array_of_properties]}; %check later
        chunk_data ->
            {chunk_data, [int, int, bool, ushort, varint, chunk]};
        multi_block_change ->
            {multi_block_change, [int, int, array_of_short]}; %check later
        block_change ->
            {block_change, [position, varint]};
        block_action ->
            {block_action, [position, ubyte, ubyte, varint]};
        block_break_animation ->
            {block_break_animation, [varint, position, byte]};
        map_chunk_bulk ->
            {map_chunk_bulk, [bool, varint, array, array_of_chunk]}; %check later
        explosion ->
            {explosion, [float, float, float, float, int, array_of_byte, float, float, float]}; %check later
        effect ->
            {effect, [int, position, int, bool]};
        sound_effect ->
            {sound_effect, [string, int, int, int, float, ubyte]};
        particle ->
            {particle, [int, bool, float, float, float, float, float, float, float, int, array_of_varint]}; %check later
        change_game_state ->
            {change_game_state, [ubyte, float]};
        spawn_global_entity ->
            {spawn_global_entity, [varint, byte, int, int, int]};
        open_window ->
            {open_window, [ubyte, string, chat, ubyte, oint]};
        close_window ->
            {close_window, [ubyte]};
        set_slot ->
            {set_slot, [byte, short, slot]};
        window_items ->
            {window_items, [ubyte, short, array_of_slot]}; %check later
        window_property ->
            {window_property, [ubyte, short, short]};
        confirm_transaction ->
            {confirm_transaction, [byte, short, bool]};
        update_sign ->
            {update_sign, [position, chat, chat, chat, chat]};
        map ->
            {map, [varint, byte, varint, array_of_byte, byte, obyte, obyte, obyte, ovarint, o_array_of_bytes]}; %check later
        update_block_entity ->
            {update_block_entity, [position, ubyte, o_nbt_tag]};
        open_sign_editor ->
            {open_sign_editor, [position]};
        statistics ->
            {statistics, [varint, array]}; %check later
        player_list_item ->
            {player_list_item, [varint, varint, array]}; %check_later
        player_abilities ->
            {player_abilities, [byte, float, float]};
        tab_complete ->
            {tab_complete, [varint, array_of_string]}; %check later
        scoreboard_objective ->
            {scoreboard_objective, [string, byte, ostring, ostring]};
        update_score ->
            {update_score, [string, byte, string, ovarint]};
        display_scoreboard ->
            {display_scoreboard, [byte, string]};
        teams ->
            {teams, [string, byte, ostring, ostring, ostring, obyte, ostring, obyte, ovarint, o_array_of_string]}; %check later
        plugin_message ->
            {plugin_message, [string, byte_array]}; 
        disconnect ->
            {disconnect, [chat]};
        server_difficulty ->
            {server_difficulty, [ubyte]};
        combat_event ->
            {combat_event, [varint, ovarint, ovarint, oint, string]};
        camera ->
            {camera, [varint]};
        world_border ->
            {world_border, [varint, double, double, double, varlong, double, double, double, double, double, double, varlong, varint, varint, varint, varint]};
        title ->
            {title, [varint, chat, chat, int, int, int]};
        set_compression ->
            {set_compression, [varint]}; %The packet Set Compression (Login, 0x03, clientbound) should be used instead.
        player_list_header_and_footer ->
            {player_list_header_and_footer, [chat, chat]};
        resource_pack_send ->
            {resource_pack_send, [string, string]};
        update_entity_nbt ->
            {update_entity_nbt, [varint, nbt_tag]}
        end.

get_messages_serverbound(ID) ->
    case ID of 
        login_start ->
            {login_start, [string]};
        encryption_response ->
            {encryption_response, [varint, byte_array, varint, byte_array]};
        handshake ->
            {handshake, [varint, string, ushort, varint]};
        keep_alive ->
            {keep_alive, [varint]};
        chat_message ->
            {chat_message, [string]};
        use_entity ->
            {use_entity, [varint, varint, ofloat, ofloat, ofloat]};
        player ->
            {player, [bool]};
        player_position ->
            {player_position, [double, double, double, bool]};
        player_look ->
            {player_look, [float, float, bool]};
        player_position_and_look ->
            {player_position_and_look, [double, double, double, float, float, bool]};
        player_digging ->
            {player_digging, [byte, position, byte]};
        block_placement ->
            {block_placement, [position, byte, slot, byte, byte, byte]};
        held_item_change ->
            {held_item_change, [short]};
        animation ->
            {animation, []};
        entity_action ->
            {entity_action, [varint, varint, varint]};
        steer_vehicle ->
            {steer_vehicle, [float, float, ubyte]};
        close_window ->
            {close_window, [ubyte]};
        click_window ->
            {click_window, [ubyte, short, byte, short, byte_enum, slot]};
        confirm_transaction ->
            {confirm_transaction, [byte, short, bool]};
        creative_inventory_action ->
            {creative_inventory_action, [short, slot]};
        enchant_item ->
            {enchant_item, [byte, byte]};
        update_sign ->
            {update_sign, [position, chat, chat, chat, chat]};
        player_abilities ->
            {player_abilities, [byte, float, float]};
        tab_complete ->
            {tab_complete, [string, bool, oposition]};
        client_settings ->
            {client_settings, [string, byte, byte, bool, ubyte]};
        client_status ->
            {client_status, [varint]};
        plugin_message ->
            {plugin_message, [string, byte_array_plugin]};
        spectate ->
            {spectate, [uuid]};
        resource_pack_status ->
            {resource_pack_status, [string, varint_enum]}
        end.



get_handshake_packet_name(ID) ->
    handshake.

get_login_packet_name_clientbound(ID) ->
    case ID of 
        0 -> disconnect;
        1 -> encryption_request;
        2 -> login_success;
        3 -> set_compression
        
    end.

get_login_packet_name_serverbound(ID) ->
    case ID of
        0 -> login_start;
        1 -> encryption_response
    end.

get_packet_number_clientbound(ID) ->
    case ID of
        disconnect -> 0;
        encryption_request -> 1;
        login_success -> 2;
        set_compression -> 3;
        keep_alive -> 0;
        join_game -> 1;
        chat_message -> 2;
        time_update -> 3;
        entity_equipment -> 4;
        spawn_position -> 5;
        update_health -> 6;
        respawn -> 7;
        player_position_and_look -> 8;
        held_item_change -> 9;
        use_bed -> 10;
        animation -> 11;
        spawn_player -> 12;
        collect_item -> 13;
        spawn_object -> 14;
        spawn_mob -> 15;
        spawn_painting -> 16;
        spawn_experience_orb -> 17;
        entity_velocity -> 18;
        destroy_entities -> 19;
        entity -> 20;
        entity_relative_move -> 21;
        entity_look_and_relative_move -> 22;
        entity_look -> 23;
        entity_teleport -> 24;
        entity_head_look -> 25;
        entity_status -> 26;
        attach_entity -> 27;
        entity_metadata -> 28;
        entity_effect -> 29;
        remove_entity_effect -> 30;
        set_experience -> 31;
        entity_properties -> 32;
        chunk_data -> 33;
        multi_block_change -> 34;
        block_change -> 35;
        block_action -> 36;
        block_break_animation -> 37;
        map_chunk_bulk -> 38;
        explosion -> 39;
        effect -> 40;
        sound_effect -> 41;
        particle -> 42;
        change_game_state -> 43;
        spawn_global_entity -> 44;
        open_window -> 45;
        close_window -> 46;
        set_slot -> 47;
        window_items -> 48;
        window_property -> 49;
        confirm_transaction -> 50;
        update_sign -> 51;
        maps -> 52;
        update_block_entity -> 53;
        open_sign_editor -> 54;
        statistics -> 55;
        player_list_item -> 56;
        player_abilities -> 57;
        tab_complete -> 58;
        score_board_objective -> 59;
        update_score -> 60;
        display_scoreboard -> 61;
        teams -> 62;
        plugin_message -> 63;
        disconnect -> 64;
        server_difficulty -> 65;
        combat_event -> 66;
        camera -> 67;
        world_border -> 68;
        title -> 69;
        set_compression -> 70;
        player_list_header_and_footer -> 71;
        resource_pack_send -> 72;
        update_entity_nbt -> 73;
        _ -> unknown
    end.
get_play_packet_name_clientbound(ID) ->
    case ID of 
        0 -> keep_alive;
        1 -> join_game;
        2 -> chat_message;
        3 -> time_update;
        4 -> entity_equipment;
        5 -> spawn_position;
        6 -> update_health;
        7 -> respawn;
        8 -> player_position_and_look;
        9 -> held_item_change;
        10 -> use_bed;
        11 -> animation;
        12 -> spawn_player;
        13 -> collect_item;
        14 -> spawn_object;
        15 -> spawn_mob;
        16 -> spawn_painting;
        17 -> spawn_experience_orb;
        18 -> entity_velocity;
        19 -> destroy_entities;
        20 -> entity;
        21 -> entity_relative_move;
        22 -> entity_look_and_relative_move;
        23 -> entity_look;
        24 -> entity_teleport;
        25 -> entity_head_look;
        26 -> entity_status;
        27 -> attach_entity;
        28 -> entity_metadata;
        29 -> entity_effect;
        30 -> remove_entity_effect;
        31 -> set_experience;
        32 -> entity_properties;
        33 -> chunk_data;
        34 -> multi_block_change;
        35 -> block_change;
        36 -> block_action;
        37 -> block_break_animation;
        38 -> map_chunk_bulk;
        39 -> explosion;
        40 -> effect;
        41 -> sound_effect;
        42 -> particle;
        43 -> change_game_state;
        44 -> spawn_global_entity;
        45 -> open_window;
        46 -> close_window;
        47 -> set_slot;
        48 -> window_items;
        49 -> window_property;
        50 -> confirm_transaction;
        51 -> update_sign;
        52 -> maps;
        53 -> update_block_entity;
        54 -> open_sign_editor;
        55 -> statistics;
        56 -> player_list_item;
        57 -> player_abilities;
        58 -> tab_complete;
        59 -> score_board_objective;
        60 -> update_score;
        61 -> display_scoreboard;
        62 -> teams;
        63 -> plugin_message;
        64 -> disconnect;
        65 -> server_difficulty;
        66 -> combat_event;
        67 -> camera;
        68 -> world_border;
        69 -> title;
        70 -> set_compression;
        71 -> player_list_header_and_footer;
        72 -> resource_pack_send;
        73 -> update_entity_nbt;
        _ -> unknown
    end.

        

get_play_packet_name_serverbound(ID) ->
    case ID of 
        0 -> keep_alive;
        1 -> chat_message;
        2 -> use_entity;
        3 -> player;
        4 -> player_position;
        5 -> player_look;
        6 -> player_position_and_look;
        7 -> player_digging;
        8 -> block_placement;
        9 -> held_item_change;
        10 -> animation;
        11 -> entity_action;
        12 -> steer_vehicle;
        13 -> close_window;
        14 -> click_window;
        15 -> confirm_transaction;
        16 -> creative_inventory_action;
        17 -> enchant_item;
        18 -> update_sign;
        19 -> player_abilities;
        20 -> tab_complete;
        21 -> client_settings;
        22 -> client_status;
        23 -> plugin_message;
        24 -> spectate;
        25 -> resource_pack_status;
        _ -> unknown
    end.