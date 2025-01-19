-module(data_packets).
-export([get_by_packet_id_clientbound/1, get_by_packet_id_serverbound/1]).

get_by_packet_id_clientbound(Id) ->
    case Id of 
        0 ->
            {0, keep_alive, [varint]};
        1 ->
            {1, join_game, [int, ubyte, byte, ubyte, string, bool]};
        2 ->
            {2, chat_message, [chat, byte]};
        3 ->
            {3, time_update, [long, long]};
        4 ->
            {4, entity_equipment, [varint, short, slot]};
        5 ->
            {5, spawn_position, [position]};
        6 ->
            {6, update_health, [float, varint, float]};
        7 ->
            {7, respawn, [int, ubyte, ubyte, string]};
        8 ->
            {8, player_position_and_look, [double, double, double, float, float, byte]};
        9 ->
            {9, held_item_change, [slot]};
        10 ->
            {10, use_bed, [varint, position]};
        11 ->
            {11, animation, [varint, ubyte]};
        12 ->
            {12, spawn_player, [varint, uuid, int, int, int, angle, angle, short, metadata]};
        13 ->
            {13, collect_item, [varint, varint]};
        14 ->
            {14, spawn_object, [varint, byte, int, int, int, angle, angle, int, oshort, oshort, oshort]};
        15 ->
            {15, spawn_mob, [varint, ubyte, int, int, int, angle, angle, angle, short, short, short, metadata]};
        16 ->
            {16, spawn_painting, [varint, string, position, ubyte]};
        17 ->
            {17, spawn_experience_orb, [varint, int, int, int, short]};
        18 ->
            {18, entity_velocity, [varint, short, short, short]};
        19 ->
            {19, destroy_entities, [varint, array_of_varint]}; %check later
        20 ->
            {20, entity, [varint]};
        21 ->
            {21, entity_relative_move, [varint, byte, byte, byte, bool]};
        22 ->
            {22, entity_look, [varint, angle, angle, bool]};
        23 ->
            {23, entity_look_and_relative_move, [varint, byte, byte, byte, angle, angle, bool]};
        24 ->
            {24, entity_teleport, [varint, int, int, int, angle, angle, bool]};
        25 ->
            {25, entity_head_look, [varint, angle]};
        26 ->
            {26, entity_status, [int, byte]};
        27 ->
            {27, attach_entity, [int, int, bool]};
        28 ->
            {28, entity_metadata, [varint, metadata]};
        29 ->
            {29, entity_effect, [varint, byte, byte, varint, bool]};
        30 ->
            {30, remove_entity_effect, [varint, byte]};
        31 ->
            {31, set_experience, [float, varint, varint]};
        32 ->
            {32, entity_properties, [varint, int, array_of_properties]}; %check later
        33 ->
            {33, chunk_data, [int, int, bool, ushort, varint, chunk]};
        34 ->
            {34, multi_block_change, [int, int, array_of_short]}; %check later
        35 ->
            {35, block_change, [position, varint]};
        36 ->
            {36, block_action, [position, ubyte, ubyte, varint]};
        37 ->
            {37, block_break_animation, [varint, position, byte]};
        38 ->
            {38, map_chunk_bulk, [bool, varint, array, array_of_chunk]}; %check later
        39 ->
            {39, explosion, [float, float, float, float, int, array_of_byte, float, float, float]}; %check later
        40 ->
            {40, effect, [int, position, int, bool]};
        41 ->
            {41, sound_effect, [string, int, int, int, float, ubyte]};
        42 ->
            {42, particle, [int, bool, float, float, float, float, float, float, float, int, array_of_varint]}; %check later
        43 ->
            {43, change_game_state, [ubyte, float]};
        44 ->
            {44, spawn_global_entity, [varint, byte, int, int, int]};
        45 ->
            {45, open_window, [ubyte, string, chat, ubyte, oint]};
        46 ->
            {46, close_window, [ubyte]};
        47 ->
            {47, set_slot, [byte, short, slot]};
        48 ->
            {48, window_items, [ubyte, short, array_of_slot]}; %check later
        49 ->
            {49, window_property, [ubyte, short, short]};
        50 ->
            {50, confirm_transaction, [byte, short, bool]};
        51 ->
            {51, update_sign, [position, chat, chat, chat, chat]};
        52 ->
            {52, map, [varint, byte, varint, array_of_byte, byte, obyte, obyte, obyte, ovarint, o_array_of_bytes]}; %check later
        53 ->
            {53, update_block_entity, [position, ubyte, o_nbt_tag]};
        54 ->
            {54, open_sign_editor, [position]};
        55 ->
            {55, statistics, [varint, array]}; %check later
        56 ->
            {56, player_list_item, [varint, varint, array]}; %check_later
        57 ->
            {57, player_abilities, [byte, float, float]};
        58 ->
            {58, tab_complete, [varint, array_of_string]}; %check later
        59 ->
            {59, scoreboard_objective, [string, byte, ostring, ostring]};
        60 ->
            {60, update_score, [string, byte, string, ovarint]};
        61 ->
            {61, display_scoreboard, [byte, string]};
        62 ->
            {62, teams, [string, byte, ostring, ostring, ostring, obyte, ostring, obyte, ovarint, o_array_of_string]}; %check later
        63 ->
            {63, plugin_message, [string, byte_array]}; 
        64 ->
            {64, disconnect, [chat]};
        65 ->
            {65, server_difficulty, [ubyte]};
        66 ->
            {66, combat_event, [varint, ovarint, ovarint, oint, string]};
        67 ->
            {67, camera, [varint]};
        68 ->
            {68, world_border, [varint, double, double, double, varlong, double, double, double, double, double, double, varlong, varint, varint, varint, varint]};
        69 ->
            {69, title, [varint, chat, chat, int, int, int]};
        70 ->
            {70, set_compression, [varint]}; %The packet Set Compression (Login, 0x03, clientbound) should be used instead.
        71 ->
            {71, player_list_header_and_footer, [chat, chat]};
        72 ->
            {72, resource_pack_send, [string, string]};
        73 ->
            {73, update_entity_nbt, [varint, nbt_tag]}
        end.

get_by_packet_id_serverbound(Id) ->
    case Id of 
        0 ->
            {0, keep_alive, [varint]};
        1 ->
            {1, chat_message, [string]};
        2 ->
            {2, use_entity, [varint, varint, ofloat, ofloat, ofloat]};
        3 ->
            {3, player, [bool]};
        4 ->
            {4, player_position, [double, double, double, bool]};
        5 ->
            {5, player_look, [float, float, bool]};
        6 ->
            {6, player_position_and_look, [double, double, double, float, float, bool]};
        7 ->
            {7, player_digging, [byte, position, byte]};
        8 ->
            {8, block_placement, [position, byte, slot, byte, byte, byte]};
        9 ->
            {9, held_item_change, [short]};
        10 ->
            {10, animation, []};
        11 ->
            {11, entity_action, [varint, varint, varint]};
        12 ->
            {12, steer_vehicle, [float, float, ubyte]};
        13 ->
            {13, close_window, [ubyte]};
        14 ->
            {14, click_window, [ubyte, short, byte, short, byte_enum, slot]};
        15 ->
            {15, confirm_transaction, [byte, short, bool]};
        16 ->
            {16, creative_inventory_action, [short, slot]};
        17 ->
            {17, enchant_item, [byte, byte]};
        18 ->
            {18, update_sign, [position, chat, chat, chat, chat]};
        19 ->
            {19, player_abilities, [byte, float, float]};
        20 ->
            {20, tab_complete, [string, bool, oposition]};
        21 ->
            {21, client_settings, [string, byte, byte, bool, ubyte]};
        22 ->
            {22, client_status, [varint]};
        23 ->
            {23, plugin_message, [string, byte_array]};
        24 ->
            {24, spectate, [uuid]};
        25 ->
            {25, resource_pack_status, [string, varint_enum]}
        end.

