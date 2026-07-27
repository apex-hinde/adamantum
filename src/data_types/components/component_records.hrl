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


