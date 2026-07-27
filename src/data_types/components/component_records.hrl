%% Existing records (keep as-is)
-record(dye, {type, colour}).
-record(trim_pattern, {type, asset_name, template_item, description, is_decal}).
-record(trim_material, {type, suffix, overrides, description}).
-record(trim, {type, material, pattern}).
-record(block_predicate, {type = 'minecraft:block_predicate', blocks, properties, nbt, data_components, partial_data_components}).
-record(firework_explosion, {type = 'minecraft:firework_explosion', shape, colors, fade_colors, has_trail, has_twinkle}).
-record(potion_effect_detail, {type = 'minecraft:potion_effect_detail', amplifier, duration, ambient, show_particles, show_icon, hidden_effect}).
-record(potion_effect, {type = 'minecraft:potion_effect', id, details}).
-record(consume_effect, {type = 'minecraft:consume_effect', effect_type, effects, probability, diameter, sound}).
-record(instrument, {type = 'minecraft:instrument', sound_event, use_duration, range, description}).
-record(jukebox_song, {type = 'minecraft:jukebox_song', sound_event, description, duration, output}).
-record(banner_pattern, {type = 'minecraft:banner_pattern', asset_id, translation_key}).
-record(custom_data, {type = 'minecraft:custom_data', data}).
-record(max_stack_size, {type = 'minecraft:max_stack_size', max_stack_size}).
-record(max_damage, {type = 'minecraft:max_damage', max_damage}).
-record(damage, {type = 'minecraft:damage', damage}).
-record(unbreakable, {type = 'minecraft:unbreakable'}).

%% Text / identifier components
-record(custom_name, {type = 'minecraft:custom_name', name}).
-record(item_name, {type = 'minecraft:item_name', name}).
-record(item_model, {type = 'minecraft:item_model', model}).
-record(lore, {type = 'minecraft:lore', lines}).
-record(rarity, {type = 'minecraft:rarity', rarity}).
-record(tooltip_style, {type = 'minecraft:tooltip_style', style}).
-record(note_block_sound, {type = 'minecraft:note_block_sound', sound}).

%% Simple integer / boolean components
-record(repair_cost, {type = 'minecraft:repair_cost', cost}).
-record(creative_slot_lock, {type = 'minecraft:creative_slot_lock'}).
-record(enchantment_glint_override, {type = 'minecraft:enchantment_glint_override', has_glint}).
-record(glider, {type = 'minecraft:glider'}).
-record(enchantable, {type = 'minecraft:enchantable', value}).
-record(map_id, {type = 'minecraft:map_id', id}).
-record(ominous_bottle_amplifier, {type = 'minecraft:ominous_bottle_amplifier', amplifier}).
-record(potion_duration_scale, {type = 'minecraft:potion_duration_scale', scale}).
-record(additional_trade_cost, {type = 'minecraft:additional_trade_cost', cost}).
-record(minimum_attack_charge, {type = 'minecraft:minimum_attack_charge', charge}).

%% NBT-only components
-record(intangible_projectile, {type = 'minecraft:intangible_projectile', data}).
-record(map_decorations, {type = 'minecraft:map_decorations', data}).
-record(debug_stick_state, {type = 'minecraft:debug_stick_state', data}).
-record(bucket_entity_data, {type = 'minecraft:bucket_entity_data', data}).
-record(lock, {type = 'minecraft:lock', key}).
-record(container_loot, {type = 'minecraft:container_loot', data}).
-record(recipes, {type = 'minecraft:recipes', data}).

%% Color / map components
-record(dyed_color, {type = 'minecraft:dyed_color', color}).
-record(map_color, {type = 'minecraft:map_color', color}).
-record(map_post_processing, {type = 'minecraft:map_post_processing', processing_type}).
-record(base_color, {type = 'minecraft:base_color', color}).

%% Slot-based components
-record(charged_projectiles, {type = 'minecraft:charged_projectiles', projectiles}).
-record(bundle_contents, {type = 'minecraft:bundle_contents', items}).
-record(use_remainder, {type = 'minecraft:use_remainder', remainder}).
-record(container, {type = 'minecraft:container', items}).
-record(sulfur_cube_content, {type = 'minecraft:sulfur_cube_content', content}).

%% Enchantment components
-record(enchantments, {type = 'minecraft:enchantments', enchantments}).
-record(stored_enchantments, {type = 'minecraft:stored_enchantments', enchantments}).

%% Attribute modifiers
-record(attribute_modifiers, {type = 'minecraft:attribute_modifiers', modifiers}).

%% Custom model data
-record(custom_model_data, {type = 'minecraft:custom_model_data', floats, flags, strings, colors}).

%% Tooltip display
-record(tooltip_display, {type = 'minecraft:tooltip_display', hide_tooltip, hidden_components}).

%% Food and consumable
-record(food, {type = 'minecraft:food', nutrition, saturation_modifier, can_always_eat}).
-record(consumable, {type = 'minecraft:consumable', consume_seconds, animation, sound, has_consume_particles, effects}).

%% Cooldown / use effects
-record(use_cooldown, {type = 'minecraft:use_cooldown', seconds, cooldown_group}).
-record(use_effects, {type = 'minecraft:use_effects', can_sprint, interact_vibrations, speed_multiplier}).

%% Damage
-record(damage_type, {type = 'minecraft:damage_type', damage_type}).
-record(damage_resistant, {type = 'minecraft:damage_resistant', types}).

%% Attack
-record(attack_range, {type = 'minecraft:attack_range', min_reach, max_reach, min_creative_reach, max_creative_reach, hitbox_margin, mob_factor}).
-record(weapon, {type = 'minecraft:weapon', damage_per_attack, disable_blocking_for}).
-record(piercing_weapon, {type = 'minecraft:piercing_weapon', deals_knockback, dismounts, sound, hit_sound}).
-record(kinetic_weapon, {type = 'minecraft:kinetic_weapon', contact_cooldown_ticks, delay_ticks, dismount_conditions, knockback_conditions, damage_conditions, forward_movement, damage_multiplier, sound, hit_sound}).
-record(swing_animation, {type = 'minecraft:swing_animation', animation_type, duration}).

%% Tool
-record(tool, {type = 'minecraft:tool', rules, default_mining_speed, damage_per_block, can_destroy_blocks_in_creative}).

%% Equippable / repairable
-record(equippable, {type = 'minecraft:equippable', slot, equip_sound, model, camera_overlay, allowed_entities, dispensable, swappable, damage_on_hurt, can_be_sheared, shearing_sound}).
-record(repairable, {type = 'minecraft:repairable', items}).

%% Death protection / blocks attacks
-record(death_protection, {type = 'minecraft:death_protection', effects}).
-record(blocks_attacks, {type = 'minecraft:blocks_attacks', block_delay_seconds, disable_cooldown_scale, damage_reductions, bypassed_by, block_sound, disable_sound}).

%% Potion contents
-record(potion_contents, {type = 'minecraft:potion_contents', potion_id, custom_color, custom_effects, custom_name}).

%% Suspicious stew
-record(suspicious_stew_effects, {type = 'minecraft:suspicious_stew_effects', effects}).

%% Book content
-record(writable_book_content, {type = 'minecraft:writable_book_content', pages}).
-record(written_book_content, {type = 'minecraft:written_book_content', raw_title, filtered_title, author, generation, pages, resolved}).

%% Entity data
-record(entity_data, {type = 'minecraft:entity_data', entity_type, data}).
-record(block_entity_data, {type = 'minecraft:block_entity_data', block_entity_type, data}).

%% Trim material provider
-record(provides_trim_material, {type = 'minecraft:provides_trim_material', key}).

%% Fireworks
-record(fireworks, {type = 'minecraft:fireworks', flight_duration, explosions}).

%% Profile / lodestone
-record(profile_component, {type = 'minecraft:profile', profile}).
-record(lodestone_tracker, {type = 'minecraft:lodestone_tracker', has_global_position, dimension, position, tracked}).

%% Pot decorations / block state / bees
-record(pot_decorations, {type = 'minecraft:pot_decorations', decorations}).
-record(block_state, {type = 'minecraft:block_state', properties}).
-record(bees, {type = 'minecraft:bees', bees}).

%% Break sound
-record(break_sound, {type = 'minecraft:break_sound', sound_event}).

%% Generic variant wrapper (for all variant/color varint components)
-record(variant_component, {type, variant}).
