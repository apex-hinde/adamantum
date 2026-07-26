%%-record(bool, {bool}).
%%-record(byte, {byte}).
%%-record(ubyte, {ubyte}).
%%-record(short, {short}).
%%-record(ushort, {ushort}).
%%-record(int, {int}).
%%-record(long, {long}).
%%-record(float, {float}).
%%-record(double, {double}).
%%-record(string, {string}).
%%-record(text_componant, {component_map}).
%%-record(json_text_componant, {json_component_map}).
%%-record(identifier, {identifier}).
%%-record(varint, {varint}).
%%-record(varlong, {varlong}).
%%-record(entity_metadata, {}). %todo
%%-record(slot, {item_count, itemID, components_to_add, components_to_remove}).
%%-record(hashed_slot, {item_count, itemID, components_to_add, components_to_remove}).
%%-record(nbt, {nbt}).
%%-record(position, {x, y, z}).
%%-record(angle, {angle}).
%%-record(uuid, {uuid}).
%%-record(bitset, {bitset}).
%%-record(fixed_bitset, {fixed_bitset}).
%%-record(optional, {fixed_bitset}).
%%-record(prefixed_optional, {fixed_bitset}).
%%-record(array, {array}).
%%-record(prefixed_array, {prefixed_array}).
%%-record(enum, {enum}).
%%-record(byte_array, {byte_array}).
%%-record(id_or_x, {id_or_x}).
%%-record(id_set, {id_set}).
%%-record(sound_event, {sound_name, has_fixed_value, fixed_range}).
%%-record(chat_type, {}).
%%-record(teleport_flags, {flagsmap}).
%%-record(light_data, {}).
%%-record(either_x_or_y, {x, y}).
%%-record(game_profile, {}).
%%-record(resolvable_profile, {}).
%%-record(debug_subscription_event, {}).
%%-record(lp_vec3, {}).



%% slot display records
-record(empty, {type}).
-record(any_fuel, {type}).
-record(with_any_potion, {type, base}).
-record(only_with_component, {type, base, component_type_id}).
-record(item, {type, item_type}).
-record(item_stack, {type, item_stack}).
-record(tag, {type, tag}).
-record(dyed, {type, dye, target}).
-record(smithing_trim, {type, base, material, pattern}).
-record(with_remainder, {type, ingredient, remainder}).
-record(composite, {type, options_count, options}).


%% recipe display records
-record(crafting_shapeless, {type, ingredients_count, ingredients, result, crafting_station}).
-record(crafting_shaped, {type, width, height, ingredients_count, ingredients, result, crafting_station}).
-record(furnace, {type, ingredient, fuel, result, crafting_station, cooking_time, experience}).
-record(stonecutter, {type, ingredient, result, crafting_station}).
-record(smithing, {type, template, base, addition, result, crafting_station}).

%% debug subscription data records
-record(dedicated_server_tick_time, {type}).
-record(bee, {type, hive_position, flower_position, travel_ticks, blacklisted_hives}).
-record(villager_brain, {type, name, profession, xp, health, max_health, inventory, wants_golem, anger_level, activities, behaviors, memories, gossips, pois, potential_pois}).
-record(breeze, {type, attack_target, jump_target}).
-record(goal_selector, {type, priority, is_running, name}).
-record(entity_path, {type, reached, next_block_index, block_position, nodes, target_nodes, open_set, closed_set, max_node_distance}).
-record(entity_block_intersection, {type, id}).
-record(bee_hive, {type, hive_type, occupant_count, honey_level, sedated}).
-record(poi, {type, position, poi_type, free_ticket_count}).
-record(redstone_wire_orientation, {type, id}).
-record(village_section, {type}).
-record(raid, {type, positions}).
-record(structure, {type, structures}).
-record(game_event_listener, {type, listener_radius}).
-record(neighbor_update, {type, position}).
-record(game_event, {type, event, x, y, z}).