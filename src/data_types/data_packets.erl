-module(data_packets).
-export([get_messages_serverbound/1, get_messages_clientbound/1]).
get_messages_serverbound(Id) ->
    case Id of 
        %%Handshake State
        'minecraft:intention' ->
            {'minecraft:intention', [varint, string, ushort, {enum, varint}]};

        %%Login State
        'minecraft:hello' ->
            {'minecraft:hello', [string, uuid]};
        'minecraft:key' ->
            {'minecraft:key', [{prefixed_array, byte}, {prefixed_array, byte}]};
        'minecraft:custom_query_answer' ->
            {'minecraft:custom_query_answer', [varint, {prefixed_optional, nothing}]};
        'minecraft:login_acknowledged' ->
            {'minecraft:login_acknowledged', []};
        'minecraft:cookie_response' ->
            {'minecraft:cookie_response', [identifier, {prefixed_optional, {prefixed_array, byte}}]};

        %% Status State
        'minecraft:status_request' ->
            {'minecraft:status_request', []};
        
        %%Configuration State
        'minecraft:client_information' ->
            {'minecraft:client_information', [string, byte, {enum, varint}, bool, ubyte, {enum, varint}, bool, bool, {enum, varint}]};
        'minecraft:finish_configuration' ->
            {'minecraft:finish_configuration', []};
        'minecraft:keep_alive' ->
            {'minecraft:keep_alive', [long]};
        'minecraft:pong' ->
            {'minecraft:pong', [int]};
        'minecraft:resource_pack' ->
            {'minecraft:resource_pack', [uuid, {enum, varint}]};
        'minecraft:select_known_packs' ->
            {'minecraft:select_known_packs', [{prefixed_array, [string, string, string]}]};
        'minecraft:custom_click_action' ->
            {'minecraft:custom_click_action', [identifier, varint, nbt]};
        'minecraft:accept_code_of_conduct' ->
            {'minecraft:accept_code_of_conduct', []};
        %% Play State
        'minecraft:accept_teleportation' ->
            {'minecraft:accept_teleportation', [varint]};
        'minecraft:attack' ->
            {'minecraft:attack', [varint]};
        'minecraft:block_entity_tag_query' ->
            {'minecraft:block_entity_tag_query', [varint, position]};
        'minecraft:bundle_item_selected' ->
            {'minecraft:bundle_item_selected', [varint, varint]};
        'minecraft:change_difficulty' ->
            {'minecraft:change_difficulty', [{enum, ubyte}]};
        'minecraft:change_game_mode' ->
            {'minecraft:change_game_mode', [{enum, varint}]};
        'minecraft:chat_ack' ->
            {'minecraft:chat_ack', [varint]};
        'minecraft:chat_command' ->
            {'minecraft:chat_command', [string]};
        'minecraft:chat_command_signed' ->
            {'minecraft:chat_command_signed', [string, long, long, {prefixed_array, [string, byte_array]}, varint, fixed_bitset, byte]};
        'minecraft:chat' ->
            {'minecraft:chat', [string, long, long, {prefixed_optional, byte_array}, varint, fixed_bitset, byte]};
        'minecraft:chat_session_update' ->
            {'minecraft:chat_session_update', [uuid, long, {prefixed_array, byte}, {prefixed_array, byte}]};
        'minecraft:chunk_batch_received' ->
            {'minecraft:chunk_batch_received', [float]};
        'minecraft:client_command' ->
            {'minecraft:client_command', [{enum, varint}]};
        'minecraft:client_tick_end' ->
            {'minecraft:client_tick_end', []};
        'minecraft:command_suggestion' ->
            {'minecraft:command_suggestion', [varint, string]};
        'minecraft:configuration_acknowledged' ->
            {'minecraft:configuration_acknowledged', []};
        'minecraft:container_button_click' ->
            {'minecraft:container_button_click', [varint, varint]};
        'minecraft:container_click' ->
            {'minecraft:container_click', [varint, varint, short, byte, {enum, varint}, {prefixed_array, [short, hashed_slot]}, hashed_slot]};
        'minecraft:container_close' ->
            {'minecraft:container_close', [varint]};
        'minecraft:container_slot_state_changed' ->
            {'minecraft:container_slot_state_changed', [varint, varint, bool]};
        'minecraft:custom_payload' ->
            {'minecraft:custom_payload', [identifier, byte_array]};
        'minecraft:debug_subscription_request' ->
            {'minecraft:debug_subscription_request', [{prefixed_array, varint}]};
        'minecraft:edit_book' ->
            {'minecraft:edit_book', [varint, {prefixed_array, string}, {prefixed_optional, string}]};
        'minecraft:entity_tag_query' ->
            {'minecraft:entity_tag_query', [varint, varint]};
        'minecraft:interact' ->
            {'minecraft:interact', [varint, {enum, varint}, lp_vec3, bool]};
        'minecraft:jigsaw_generate' ->
            {'minecraft:jigsaw_generate', [position, varint, bool]};
        'minecraft:lock_difficulty' ->
            {'minecraft:lock_difficulty', [bool]};
        'minecraft:move_player_pos' ->
            {'minecraft:move_player_pos', [double, double, double, byte]};
        'minecraft:move_player_pos_rot' ->
            {'minecraft:move_player_pos_rot', [double, double, double, float, float, byte]};
        'minecraft:move_player_rot' ->
            {'minecraft:move_player_rot', [float, float, byte]};
        'minecraft:move_player_status_only' ->
            {'minecraft:move_player_status_only', [byte]};
        'minecraft:move_vehicle' ->
            {'minecraft:move_vehicle', [double, double, double, float, float, bool]};
        'minecraft:paddle_boat' ->
            {'minecraft:paddle_boat', [bool, bool]};
        'minecraft:pick_item_from_block' ->
            {'minecraft:pick_item_from_block', [position, bool]};
        'minecraft:pick_item_from_entity' ->
            {'minecraft:pick_item_from_entity', [varint, bool]};
        'minecraft:ping_request' ->
            {'minecraft:ping_request', [long]};
        'minecraft:place_recipe' ->
            {'minecraft:place_recipe', [varint, varint, bool]};
        'minecraft:player_abilities' ->
            {'minecraft:player_abilities', [byte]};
        'minecraft:player_action' ->
            {'minecraft:player_action', [{enum, varint}, position, {enum, byte}, varint]};
        'minecraft:player_command' ->
            {'minecraft:player_command', [varint, {enum, varint}, varint]};
        'minecraft:player_input' ->
            {'minecraft:player_input', [ubyte]};
        'minecraft:player_loaded' ->
            {'minecraft:player_loaded', []};
        'minecraft:recipe_book_change_settings' ->
            {'minecraft:recipe_book_change_settings', [{enum, varint}, bool, bool]};
        'minecraft:recipe_book_seen_recipe' ->
            {'minecraft:recipe_book_seen_recipe', [varint]};
        'minecraft:rename_item' ->
            {'minecraft:rename_item', [string]};
        'minecraft:seen_advancements' ->
            {'minecraft:seen_advancements', [seen_advancements]};
        'minecraft:select_trade' ->
            {'minecraft:select_trade', [varint]};
        'minecraft:set_beacon' ->
            {'minecraft:set_beacon', [{prefixed_optional, varint}, {prefixed_optional, varint}]};
        'minecraft:set_carried_item' ->
            {'minecraft:set_carried_item', [short]};
        'minecraft:set_command_block' ->
            {'minecraft:set_command_block', [position, string, {enum, varint}, byte]};
        'minecraft:set_command_minecart' ->
            {'minecraft:set_command_minecart', [varint, string, bool]};
        'minecraft:set_creative_mode_slot' ->
            {'minecraft:set_creative_mode_slot', [short, slot]};
        'minecraft:set_game_rule' ->
            {'minecraft:set_game_rule', [{prefixed_array, [identifier, string]}]};
        'minecraft:set_jigsaw_block' ->
            {'minecraft:set_jigsaw_block', [position, identifier, identifier, identifier, string, string, varint, varint]};
        'minecraft:set_structure_block' ->
            {'minecraft:set_structure_block', [position, {enum, varint}, {enum, varint}, string, byte, byte, byte, byte, byte, byte, {enum, varint}, {enum, varint}, string, float, varlong, byte]};
        'minecraft:set_test_block' ->
            {'minecraft:set_test_block', [position, {enum, varint}, string]};
        'minecraft:sign_update' ->
            {'minecraft:sign_update', [position, bool, string, string, string, string]};
        'minecraft:spectator_action' ->
            {'minecraft:spectator_action', [varint]};
        'minecraft:swing' ->
            {'minecraft:swing', [{enum, varint}]};
        'minecraft:teleport_to_entity' ->
            {'minecraft:teleport_to_entity', [uuid]};
        'minecraft:test_instance_block_action' ->
            {'minecraft:test_instance_block_action', [position, {enum, varint}, {prefixed_optional, identifier}, varint, varint, varint, {enum, varint}, bool, {enum, varint}, {prefixed_optional, text_component}]};
        'minecraft:use_item_on' ->
            {'minecraft:use_item_on', [{enum, varint}, position, {enum, varint}, float, float, float, bool, bool, varint]};
        'minecraft:use_item' ->
            {'minecraft:use_item', [{enum, varint}, varint, float, float]};
        _ ->
            error({unregistered_packet, Id})
        end.
        
        
get_messages_clientbound(Id) ->
    case Id of 
        %% Login State
        'minecraft:login_disconnect' ->
            {'minecraft:login_disconnect', [json_text_componant]};
        'minecraft:hello' ->
            {'minecraft:hello', [string, {prefixed_array, byte}, {prefixed_array, byte}, bool]};
        'minecraft:login_finished' ->
            {'minecraft:login_finished', [game_profile, uuid]};
        'minecraft:login_compression' ->
            {'minecraft:login_compression', [varint]};
        'minecraft:custom_query' ->
            {'minecraft:custom_query', [varint, identifier, nothing]};
        'minecraft:cookie_request' ->
            {'minecraft:cookie_request', [identifier]};
        %% Status State
        'minecraft:status_response' ->
            {'minecraft:status_response', [string]};
        %% Configuration State
        'minecraft:custom_payload' ->
            {'minecraft:custom_payload', [identifier, nothing]};
        'minecraft:disconnect' ->
            {'minecraft:disconnect', [text_component]};
        'minecraft:finish_configuration' ->
            {'minecraft:finish_configuration', []};
        'minecraft:keep_alive' ->
            {'minecraft:keep_alive', [long]};
        'minecraft:ping' ->
            {'minecraft:ping', [int]};
        'minecraft:reset_chat' ->
            {'minecraft:reset_chat', []};
        'minecraft:registry_data' ->
            {'minecraft:registry_data', [identifier, {prefixed_array, [identifier, {prefixed_optional, nbt}]}]};
        'minecraft:resource_pack_pop' ->
            {'minecraft:resource_pack_pop', [{prefixed_optional, uuid}]};
        'minecraft:resource_pack_push' ->
            {'minecraft:resource_pack_push', [uuid, string, string, bool, {prefixed_optional, text_component}]};
        'minecraft:store_cookie' ->
            {'minecraft:store_cookie', [identifier, {prefixed_array, byte}]};
        'minecraft:transfer' ->
            {'minecraft:transfer', [string, varint]};
        'minecraft:update_enabled_features' ->
            {'minecraft:update_enabled_features', [{prefixed_array, identifier}]};
        'minecraft:update_tags' ->
            {'minecraft:update_tags', [{prefixed_array, [identifier, {prefixed_array, tag}]}]};
        'minecraft:select_known_packs' ->
            {'minecraft:select_known_packs', [{prefixed_array, [string, string, string]}]};
        'minecraft:custom_report_details' ->
            {'minecraft:custom_report_details', [{prefixed_array, [string, string]}]};
        'minecraft:server_links' ->
            {'minecraft:server_links', [{prefixed_array, [{either_x_or_y, {enum, varint}, text_component}]}]};
        'minecraft:clear_dialog' ->
            {'minecraft:clear_dialog', []};
        'minecraft:show_dialog' ->
            {'minecraft:show_dialog', [nbt]};
        'minecraft:code_of_conduct' ->
            {'minecraft:code_of_conduct', [string]};
        %% play state
        'minecraft:bundle_delimiter' ->
            {'minecraft:bundle_delimiter', []};
        'minecraft:add_entity' ->
            {'minecraft:add_entity', [varint, uuid, varint, double, double, double, lp_vec3, angle, angle, angle, varint]};
        'minecraft:animate' ->
            {'minecraft:animate', [varint, ubyte]};
        'minecraft:award_stats' ->
            {'minecraft:award_stats', [{prefixed_array, [varint, varint, varint]}]};
        'minecraft:block_changed_ack' ->
            {'minecraft:block_changed_ack', [varint]};
        'minecraft:block_destruction' ->
            {'minecraft:block_destruction', [varint, position, ubyte]};
        'minecraft:block_entity_data' ->
            {'minecraft:block_entity_data', [position, varint, nbt]};
        'minecraft:block_event' ->
            {'minecraft:block_event', [position, ubyte, ubyte, varint]};
        'minecraft:block_update' ->
            {'minecraft:block_update', [position, varint]};
        'minecraft:boss_event' ->
            {'minecraft:boss_event', [boss_bar]};
        'minecraft:change_difficulty' ->
            {'minecraft:change_difficulty', [{enum, ubyte}, bool]};
        'minecraft:chunk_batch_finished' ->
            {'minecraft:chunk_batch_finished', [varint]};
        'minecraft:chunk_batch_start' ->
            {'minecraft:chunk_batch_start', []};
        'minecraft:chunks_biomes' ->
            {'minecraft:chunks_biomes', [{prefixed_array, [int, int, {prefixed_array, byte}]}]};
        'minecraft:clear_titles' ->
            {'minecraft:clear_titles', [bool]};
        'minecraft:command_suggestions' ->
            {'minecraft:command_suggestions', [varint, varint, varint, {prefixed_array, [string, {prefixed_optional, text_component}]}]};
        'minecraft:commands' ->
            {'minecraft:commands', [{prefixed_array, node}, varint]};
        'minecraft:container_close' ->
            {'minecraft:container_close', [varint]};
        'minecraft:container_set_content' ->
            {'minecraft:container_set_content', [varint, varint, {prefixed_array, slot}, slot]};
        'minecraft:container_set_data' ->
            {'minecraft:container_set_data', [varint, short, short]};
        'minecraft:container_set_slot' ->
            {'minecraft:container_set_slot', [varint, varint, short, slot]};
        'minecraft:cooldown' ->
            {'minecraft:cooldown', [identifier, varint]};
        'minecraft:custom_chat_completions' ->
            {'minecraft:custom_chat_completions', [{enum, varint}, {prefixed_array, string}]};
        'minecraft:damage_event' ->
            {'minecraft:damage_event', [varint, varint, varint, varint, {prefixed_optional, [double, double, double]}]};
        'minecraft:debug/block_value' ->
            {'minecraft:debug/block_value', [position, debug_subscription_update]};
        'minecraft:debug/chunk_value' ->
            {'minecraft:debug/chunk_value', [int, int, debug_subscription_update]};
        'minecraft:debug/entity_value' ->
            {'minecraft:debug/entity_value', [varint, debug_subscription_update]};
        'minecraft:debug/event' ->
            {'minecraft:debug/event', [debug_subscription_event]};
        'minecraft:debug_sample' ->
            {'minecraft:debug_sample', [{prefixed_array, long}, {enum, varint}]};
        'minecraft:delete_chat' ->
            {'minecraft:delete_chat', [delete_chat]};
        'minecraft:disguised_chat' ->
            {'minecraft:disguised_chat', [text_component, {id_or_x, chat_type}, text_component, {prefixed_optional, text_component}]};
        'minecraft:entity_event' ->
            {'minecraft:entity_event', [int, {enum, byte}]};
        'minecraft:entity_position_sync' ->
            {'minecraft:entity_position_sync', [varint, double, double, double, double, double, double, float, float, bool]};
        'minecraft:explode' ->
            {'minecraft:explode', [double, double, double, float, int, {prefixed_optional, [double, double, double]}, varint, nothing, {id_or_x, sound_event}, {prefixed_array, [varint, nothing, float, float, varint]}]};
        'minecraft:forget_level_chunk' ->
            {'minecraft:forget_level_chunk', [int, int]};
        'minecraft:game_event' ->
            {'minecraft:game_event', [ubyte, float]};
        'minecraft:game_rule_values' ->
            {'minecraft:game_rule_values', [{prefixed_array, [identifier, string]}]};
        'minecraft:game_test_highlight_pos' ->
            {'minecraft:game_test_highlight_pos', [position, position]};
        'minecraft:mount_screen_open' ->
            {'minecraft:mount_screen_open', [varint, varint, int]};
        'minecraft:hurt_animation' ->
            {'minecraft:hurt_animation', [varint, float]};
        'minecraft:initialize_border' ->
            {'minecraft:initialize_border', [double, double, double, double, varlong, varint, varint, varint]};
        'minecraft:level_chunk_with_light' ->
            {'minecraft:level_chunk_with_light', [int, int, {prefixed_array, [{enum, varint}, {prefixed_array, long}]}, {prefixed_array, byte}, {prefixed_array, [ubyte, short, varint, nbt]}, light_data]};
        'minecraft:level_event' ->
            {'minecraft:level_event', [int, position, int, bool]};
        'minecraft:level_particles' ->
            {'minecraft:level_particles', [bool, bool, double, double, double, float, float, float, float, int, varint, nothing]};
        'minecraft:light_update' ->
            {'minecraft:light_update', [varint, varint, light_data]};
        'minecraft:login' ->
            {'minecraft:login', [int, bool, {prefixed_array, identifier}, varint, varint, varint, bool, bool, bool, varint, identifier, long, ubyte, byte, bool, {prefixed_optional, [identifier, position]}, varint, varint, bool, bool]};
        'minecraft:low_disk_space_warning' ->
            {'minecraft:low_disk_space_warning', []};
        'minecraft:map_item_data' ->
            {'minecraft:map_item_data', [varint, byte, bool, {prefixed_optional, {prefixed_array, [{enum, varint}, byte, byte, byte, {prefixed_optional, text_component}]}}, ubyte, {prefixed_optional, [ubyte, ubyte, ubyte, {prefixed_array, ubyte}]}]};
        'minecraft:merchant_offers' ->
            {'minecraft:merchant_offers', [varint, {prefixed_array, [varint, varint, {prefixed_array, [{enum, varint}, nothing]}, slot, {prefixed_optional, {prefixed_array, [{enum, varint}, nothing]}}, bool, int, int, int, int, float, int]}, varint, varint, bool, bool]};
        'minecraft:move_entity_pos' ->
            {'minecraft:move_entity_pos', [varint, short, short, short, bool]};
        'minecraft:move_entity_pos_rot' ->
            {'minecraft:move_entity_pos_rot', [varint, short, short, short, angle, angle, bool]};
        'minecraft:move_minecart_along_track' ->
            {'minecraft:move_minecart_along_track', [varint, {prefixed_array, [double, double, double, double, double, double, angle, angle, float]}]};
        'minecraft:move_entity_rot' ->
            {'minecraft:move_entity_rot', [varint, angle, angle, bool]};
        'minecraft:move_vehicle' ->
            {'minecraft:move_vehicle', [double, double, double, float, float]};
        'minecraft:open_book' ->
            {'minecraft:open_book', [{enum, varint}]};
        'minecraft:open_screen' ->
            {'minecraft:open_screen', [varint, varint, text_component]};
        'minecraft:open_sign_editor' ->
            {'minecraft:open_sign_editor', [position, bool]};
        'minecraft:place_ghost_recipe' ->
            {'minecraft:place_ghost_recipe', [varint, recipe_display]};
        'minecraft:player_abilities' ->
            {'minecraft:player_abilities', [byte, float, float]};
        'minecraft:player_chat' ->
            {'minecraft:player_chat', [varint, uuid, varint, {prefixed_optional, byte_array}, string, long, long, delete_chat, {prefixed_optional, text_component}, {enum, varint}, {prefixed_optional, bitset}, {id_or_x, chat_type}, text_component, {prefixed_optional, text_component}]};
        'minecraft:player_combat_end' ->
            {'minecraft:player_combat_end', [varint]};
        'minecraft:player_combat_enter' ->
            {'minecraft:player_combat_enter', []};
        'minecraft:player_combat_kill' ->
            {'minecraft:player_combat_kill', [varint, text_component]};
        'minecraft:player_info_remove' ->
            {'minecraft:player_info_remove', [{prefixed_array, uuid}]};
        'minecraft:player_info_update' ->
            {'minecraft:player_info_update', [player_info_update]};
        'minecraft:player_look_at' ->
            {'minecraft:player_look_at', [{enum, varint}, double, double, double, {prefixed_optional, [varint, {enum, varint}]}]};
        'minecraft:player_position' ->
            {'minecraft:player_position', [varint, double, double, double, double, double, double, float, float, teleport_flags]};
        'minecraft:player_rotation' ->
            {'minecraft:player_rotation', [float, bool, float, bool]};
        'minecraft:recipe_book_add' ->
            {'minecraft:recipe_book_add', [{prefixed_array, [varint, recipe_display, varint, varint, {prefixed_optional, {prefixed_array, id_set}}, byte]}, bool]};
        'minecraft:recipe_book_remove' ->
            {'minecraft:recipe_book_remove', [{prefixed_array, varint}]};
        'minecraft:recipe_book_settings' ->
            {'minecraft:recipe_book_settings', [bool, bool, bool, bool, bool, bool, bool, bool]};
        'minecraft:remove_entities' ->
            {'minecraft:remove_entities', [{prefixed_array, varint}]};
        'minecraft:remove_mob_effect' ->
            {'minecraft:remove_mob_effect', [varint, varint]};
        'minecraft:reset_score' ->
            {'minecraft:reset_score', [string, {prefixed_optional, string}]};
        'minecraft:respawn' ->
            {'minecraft:respawn', [varint, identifier, long, ubyte, byte, bool, bool, {prefixed_optional, [identifier, position]}, varint, varint, byte]};
        'minecraft:rotate_head' ->
            {'minecraft:rotate_head', [varint, angle]};
        'minecraft:section_blocks_update' ->
            {'minecraft:section_blocks_update', [long, {prefixed_array, varlong}]};
        'minecraft:select_advancements_tab' ->
            {'minecraft:select_advancements_tab', [{prefixed_optional, identifier}]};
        'minecraft:server_data' ->
            {'minecraft:server_data', [text_component, {prefixed_optional, {prefixed_array, byte}}]};
        'minecraft:set_action_bar_text' ->
            {'minecraft:set_action_bar_text', [text_component]};
        'minecraft:set_border_center' ->
            {'minecraft:set_border_center', [double, double]};
        'minecraft:set_border_lerp_size' ->
            {'minecraft:set_border_lerp_size', [double, double, varlong]};
        'minecraft:set_border_size' ->
            {'minecraft:set_border_size', [double]};
        'minecraft:set_border_warning_delay' ->
            {'minecraft:set_border_warning_delay', [varint]};
        'minecraft:set_border_warning_distance' ->
            {'minecraft:set_border_warning_distance', [varint]};
        'minecraft:set_camera' ->
            {'minecraft:set_camera', [varint]};
        'minecraft:set_chunk_cache_center' ->
            {'minecraft:set_chunk_cache_center', [varint, varint]};
        'minecraft:set_chunk_cache_radius' ->
            {'minecraft:set_chunk_cache_radius', [varint]};
        'minecraft:set_cursor_item' ->
            {'minecraft:set_cursor_item', [slot]};
        'minecraft:set_default_spawn_position' ->
            {'minecraft:set_default_spawn_position', [identifier, position, float, float]};
        'minecraft:set_display_objective' ->
            {'minecraft:set_display_objective', [varint, string]};
        'minecraft:set_entity_data' ->
            {'minecraft:set_entity_data', [varint, entity_metadata]};
        'minecraft:set_entity_link' ->
            {'minecraft:set_entity_link', [int, int]};
        'minecraft:set_entity_motion' ->
            {'minecraft:set_entity_motion', [varint, lp_vec3]};
        'minecraft:set_equipment' ->
            {'minecraft:set_equipment', [set_equipment]};
        'minecraft:set_experience' ->
            {'minecraft:set_experience', [float, varint, varint]};
        'minecraft:set_health' ->
            {'minecraft:set_health', [float, varint, float]};
        'minecraft:set_held_slot' ->
            {'minecraft:set_held_slot', [varint]};
        'minecraft:set_objective' ->
            {'minecraft:set_objective', [set_objective]};
        'minecraft:set_passengers' ->
            {'minecraft:set_passengers', [varint, {prefixed_array, varint}]};
        'minecraft:set_player_inventory' ->
            {'minecraft:set_player_inventory', [varint, slot]};
        'minecraft:set_player_team' ->
            {'minecraft:set_player_team', [set_player_team]};
        'minecraft:set_score' ->
            {'minecraft:set_score', [set_score]};
        'minecraft:set_simulation_distance' ->
            {'minecraft:set_simulation_distance', [varint]};
        'minecraft:set_subtitle_text' ->
            {'minecraft:set_subtitle_text', [text_component]};
        'minecraft:set_time' ->
            {'minecraft:set_time', [long, {prefixed_array, [varint, varlong, float, float]}]};
        'minecraft:set_title_text' ->
            {'minecraft:set_title_text', [text_component]};
        'minecraft:set_titles_animation' ->
            {'minecraft:set_titles_animation', [int, int, int]};
        'minecraft:sound_entity' ->
            {'minecraft:sound_entity', [{id_or_x, sound_event}, {enum, varint}, varint, float, float, long]};
        'minecraft:sound' ->
            {'minecraft:sound', [{id_or_x, sound_event}, {enum, varint}, int, int, int, float, float, long]};
        'minecraft:start_configuration' ->
            {'minecraft:start_configuration', []};
        'minecraft:stop_sound' ->
            {'minecraft:stop_sound', [stop_sound]};
        'minecraft:system_chat' ->
            {'minecraft:system_chat', [text_component, bool]};
        'minecraft:tab_list' ->
            {'minecraft:tab_list', [text_component, text_component]};
        'minecraft:tag_query' ->
            {'minecraft:tag_query', [varint, nbt]};
        'minecraft:take_item_entity' ->
            {'minecraft:take_item_entity', [varint, varint, varint]};
        'minecraft:teleport_entity' ->
            {'minecraft:teleport_entity', [varint, double, double, double, double, double, double, float, float, teleport_flags, bool]};
        'minecraft:test_instance_block_status' ->
            {'minecraft:test_instance_block_status', [text_component, {optional, [double, double, double]}]};
        'minecraft:ticking_state' ->
            {'minecraft:ticking_state', [float, bool]};
        'minecraft:ticking_step' ->
            {'minecraft:ticking_step', [varint]};
        'minecraft:update_advancements' ->
            {'minecraft:update_advancements', [update_advancements]};
        'minecraft:update_attributes' ->
            {'minecraft:update_attributes', [varint, {prefixed_array, [varint, double, {prefixed_array, [identifier, double, byte]}]}]};
        'minecraft:update_mob_effect' ->
            {'minecraft:update_mob_effect', [varint, varint, varint, varint, byte]};
        'minecraft:update_recipes' ->
            {'minecraft:update_recipes', [{prefixed_array, [identifier, {prefixed_array, varint}]}, {prefixed_array, [id_set, slot_display]}]};
        'minecraft:projectile_power' ->
            {'minecraft:projectile_power', [varint, double]};
        'minecraft:waypoint' ->
            {'minecraft:waypoint', [{enum, varint}, {either_x_or_y, uuid, string}, identifier, {prefixed_optional, [ubyte, ubyte, ubyte]}, waypoint_data]};
        'minecraft:clear_dialog' ->
            {'minecraft:clear_dialog', []};
        'minecraft:show_dialog' ->
            {'minecraft:show_dialog', [{id_or_x, nbt}]};
        _ ->
            {error, Id}
    end.