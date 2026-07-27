-module(component_decode).

-export([
	 decode_component/2
	]).


-include("src/data_types/components/component_records.hrl").
-include("src/data_types/records.hrl").

decode_component(TypeId, BinData) when is_integer(TypeId), is_binary(BinData) ->
    Name = component_type_registry:id_to_name(TypeId),
    decode(Name, BinData);
decode_component(Name, BinData) when is_atom(Name), is_binary(BinData) ->
    decode(Name, BinData);
decode_component(BinData, TypeId) when is_binary(BinData), (is_atom(TypeId) orelse is_integer(TypeId)) ->
    decode_component(TypeId, BinData).

decode(TypeId, Data) ->
    case TypeId of
		dye ->
			decode_dye_color(Data);
		'minecraft:dye' ->
			decode_dye_color(Data);
		trim ->
			decode_trim(Data);
		'minecraft:trim' ->
			decode_trim(Data);
		trim_pattern ->
			decode_trim_pattern(Data);
		'minecraft:trim_pattern' ->
			decode_trim_pattern(Data);
		trim_material ->
			decode_trim_material(Data);
		'minecraft:trim_material' ->
			decode_trim_material(Data);
		block_predicate ->
			decode_block_predicate(Data);
		'minecraft:block_predicate' ->
			decode_block_predicate(Data);
		block_predictate ->
			decode_block_predicate(Data);
		property ->
			decode_property(Data);
		exact_data_component_matcher ->
			decode_exact_data_component_matcher(Data);
		partial_data_component_matcher ->
			decode_partial_data_component_matcher(Data);
		firework_explosion ->
			decode_firework_explosion(Data);
		'minecraft:firework_explosion' ->
			decode_firework_explosion(Data);
		potion_effect ->
			decode_potion_effect(Data);
		'minecraft:potion_effect' ->
			decode_potion_effect(Data);
		potion_effect_detail ->
			decode_potion_effect_detail(Data);
		'minecraft:potion_effect_detail' ->
			decode_potion_effect_detail(Data);
		consume_effect ->
			decode_consume_effect(Data);
		'minecraft:consume_effect' ->
			decode_consume_effect(Data);
		'minecraft:apply_effects' ->
			decode_consume_effect_variant('minecraft:apply_effects', Data);
		'minecraft:remove_effects' ->
			decode_consume_effect_variant('minecraft:remove_effects', Data);
		'minecraft:clear_all_effects' ->
			decode_consume_effect_variant('minecraft:clear_all_effects', Data);
		'minecraft:teleport_randomly' ->
			decode_consume_effect_variant('minecraft:teleport_randomly', Data);
		'minecraft:play_sound' ->
			decode_consume_effect_variant('minecraft:play_sound', Data);
		'minecraft:wolf/collar' ->
			decode_dye_color(Data);
		'minecraft:cat/collar' ->
			decode_dye_color(Data);
		'minecraft:sheep/color' ->
			decode_dye_color(Data);
		'minecraft:shulker/color' ->
			decode_dye_color(Data);
		'minecraft:tropical_fish/base_color' ->
			decode_dye_color(Data);
		'minecraft:tropical_fish/pattern_color' ->
			decode_dye_color(Data);
		'minecraft:painting/variant' ->
			decode_painting_variant(Data);
		painting_variant ->
			decode_painting_variant(Data);
		'minecraft:can_place_on' ->
			decode:decode_type(Data, {prefixed_array, block_predicate});
		'minecraft:can_break' ->
			decode:decode_type(Data, {prefixed_array, block_predicate});
		can_place_on ->
			decode:decode_type(Data, {prefixed_array, block_predicate});
		can_break ->
			decode:decode_type(Data, {prefixed_array, block_predicate});
		instrument ->
			decode_instrument(Data);
		'minecraft:instrument' ->
			decode:decode_type(Data, {id_or_x, instrument});
		jukebox_song ->
			decode_jukebox_song(Data);
		'minecraft:jukebox_song' ->
			decode_jukebox_song(Data);
		jukebox_playable ->
			decode:decode_type(Data, {id_or_x, jukebox_song});
		'minecraft:jukebox_playable' ->
			decode:decode_type(Data, {id_or_x, jukebox_song});
		banner_pattern ->
			decode_banner_pattern(Data);
		'minecraft:banner_pattern' ->
			decode_banner_pattern(Data);
		'minecraft:provides_banner_patterns' ->
			decode:decode_type(Data, id_set);
		provides_banner_patterns ->
			decode:decode_type(Data, id_set);
		'minecraft:banner_patterns' ->
			decode:decode_type(Data, {prefixed_array, [{id_or_x, banner_pattern}, dye]});
		banner_patterns ->
			decode:decode_type(Data, {prefixed_array, [{id_or_x, banner_pattern}, dye]});
		custom_data ->
			decode_custom_data(Data);
		'minecraft:custom_data' ->
			decode_custom_data(Data);
		max_stack_size ->
			decode_max_stack_size(Data);
		'minecraft:max_stack_size' ->
			decode_max_stack_size(Data);
		max_damage ->
			decode_max_damage(Data);
		'minecraft:max_damage' ->
			decode_max_damage(Data);
		damage ->
			decode_damage(Data);
		'minecraft:damage' ->
			decode_damage(Data);
		unbreakable ->
			decode_unbreakable(Data);
		'minecraft:unbreakable' ->
			decode_unbreakable(Data);

		%% Text / identifier components
		custom_name -> decode_custom_name(Data);
		'minecraft:custom_name' -> decode_custom_name(Data);
		item_name -> decode_item_name(Data);
		'minecraft:item_name' -> decode_item_name(Data);
		item_model -> decode_item_model(Data);
		'minecraft:item_model' -> decode_item_model(Data);
		lore -> decode_lore(Data);
		'minecraft:lore' -> decode_lore(Data);
		rarity -> decode_rarity(Data);
		'minecraft:rarity' -> decode_rarity(Data);
		tooltip_style -> decode_tooltip_style(Data);
		'minecraft:tooltip_style' -> decode_tooltip_style(Data);
		note_block_sound -> decode_note_block_sound(Data);
		'minecraft:note_block_sound' -> decode_note_block_sound(Data);

		%% Simple integer / boolean components
		repair_cost -> decode_repair_cost(Data);
		'minecraft:repair_cost' -> decode_repair_cost(Data);
		creative_slot_lock -> decode_creative_slot_lock(Data);
		'minecraft:creative_slot_lock' -> decode_creative_slot_lock(Data);
		enchantment_glint_override -> decode_enchantment_glint_override(Data);
		'minecraft:enchantment_glint_override' -> decode_enchantment_glint_override(Data);
		glider -> decode_glider(Data);
		'minecraft:glider' -> decode_glider(Data);
		enchantable -> decode_enchantable(Data);
		'minecraft:enchantable' -> decode_enchantable(Data);
		map_id -> decode_map_id(Data);
		'minecraft:map_id' -> decode_map_id(Data);
		ominous_bottle_amplifier -> decode_ominous_bottle_amplifier(Data);
		'minecraft:ominous_bottle_amplifier' -> decode_ominous_bottle_amplifier(Data);
		potion_duration_scale -> decode_potion_duration_scale(Data);
		'minecraft:potion_duration_scale' -> decode_potion_duration_scale(Data);
		additional_trade_cost -> decode_additional_trade_cost(Data);
		'minecraft:additional_trade_cost' -> decode_additional_trade_cost(Data);
		minimum_attack_charge -> decode_minimum_attack_charge(Data);
		'minecraft:minimum_attack_charge' -> decode_minimum_attack_charge(Data);

		%% NBT-only components
		intangible_projectile -> decode_intangible_projectile(Data);
		'minecraft:intangible_projectile' -> decode_intangible_projectile(Data);
		map_decorations -> decode_map_decorations(Data);
		'minecraft:map_decorations' -> decode_map_decorations(Data);
		debug_stick_state -> decode_debug_stick_state(Data);
		'minecraft:debug_stick_state' -> decode_debug_stick_state(Data);
		bucket_entity_data -> decode_bucket_entity_data(Data);
		'minecraft:bucket_entity_data' -> decode_bucket_entity_data(Data);
		lock -> decode_lock(Data);
		'minecraft:lock' -> decode_lock(Data);
		container_loot -> decode_container_loot(Data);
		'minecraft:container_loot' -> decode_container_loot(Data);
		recipes -> decode_recipes(Data);
		'minecraft:recipes' -> decode_recipes(Data);

		%% Color / map components
		dyed_color -> decode_dyed_color(Data);
		'minecraft:dyed_color' -> decode_dyed_color(Data);
		map_color -> decode_map_color(Data);
		'minecraft:map_color' -> decode_map_color(Data);
		map_post_processing -> decode_map_post_processing(Data);
		'minecraft:map_post_processing' -> decode_map_post_processing(Data);
		base_color -> decode_base_color(Data);
		'minecraft:base_color' -> decode_base_color(Data);

		%% Slot-based components
		charged_projectiles -> decode_charged_projectiles(Data);
		'minecraft:charged_projectiles' -> decode_charged_projectiles(Data);
		bundle_contents -> decode_bundle_contents(Data);
		'minecraft:bundle_contents' -> decode_bundle_contents(Data);
		use_remainder -> decode_use_remainder(Data);
		'minecraft:use_remainder' -> decode_use_remainder(Data);
		container -> decode_container(Data);
		'minecraft:container' -> decode_container(Data);
		sulfur_cube_content -> decode_sulfur_cube_content(Data);
		'minecraft:sulfur_cube_content' -> decode_sulfur_cube_content(Data);

		%% Enchantment components
		enchantments -> decode_enchantments(Data);
		'minecraft:enchantments' -> decode_enchantments(Data);
		stored_enchantments -> decode_stored_enchantments(Data);
		'minecraft:stored_enchantments' -> decode_stored_enchantments(Data);

		%% Attribute modifiers
		attribute_modifiers -> decode_attribute_modifiers(Data);
		'minecraft:attribute_modifiers' -> decode_attribute_modifiers(Data);

		%% Custom model data
		custom_model_data -> decode_custom_model_data(Data);
		'minecraft:custom_model_data' -> decode_custom_model_data(Data);

		%% Tooltip display
		tooltip_display -> decode_tooltip_display(Data);
		'minecraft:tooltip_display' -> decode_tooltip_display(Data);

		%% Food and consumable
		food -> decode_food(Data);
		'minecraft:food' -> decode_food(Data);
		consumable -> decode_consumable(Data);
		'minecraft:consumable' -> decode_consumable(Data);

		%% Cooldown / use effects
		use_cooldown -> decode_use_cooldown(Data);
		'minecraft:use_cooldown' -> decode_use_cooldown(Data);
		use_effects -> decode_use_effects(Data);
		'minecraft:use_effects' -> decode_use_effects(Data);

		%% Damage
		damage_type -> decode_damage_type(Data);
		'minecraft:damage_type' -> decode_damage_type(Data);
		damage_resistant -> decode_damage_resistant(Data);
		'minecraft:damage_resistant' -> decode_damage_resistant(Data);

		%% Attack
		attack_range -> decode_attack_range(Data);
		'minecraft:attack_range' -> decode_attack_range(Data);
		weapon -> decode_weapon(Data);
		'minecraft:weapon' -> decode_weapon(Data);
		piercing_weapon -> decode_piercing_weapon(Data);
		'minecraft:piercing_weapon' -> decode_piercing_weapon(Data);
		kinetic_weapon -> decode_kinetic_weapon(Data);
		'minecraft:kinetic_weapon' -> decode_kinetic_weapon(Data);
		swing_animation -> decode_swing_animation(Data);
		'minecraft:swing_animation' -> decode_swing_animation(Data);

		%% Tool
		tool -> decode_tool(Data);
		'minecraft:tool' -> decode_tool(Data);

		%% Equippable / repairable
		equippable -> decode_equippable(Data);
		'minecraft:equippable' -> decode_equippable(Data);
		repairable -> decode_repairable(Data);
		'minecraft:repairable' -> decode_repairable(Data);

		%% Death protection / blocks attacks
		death_protection -> decode_death_protection(Data);
		'minecraft:death_protection' -> decode_death_protection(Data);
		blocks_attacks -> decode_blocks_attacks(Data);
		'minecraft:blocks_attacks' -> decode_blocks_attacks(Data);

		%% Potion contents
		potion_contents -> decode_potion_contents(Data);
		'minecraft:potion_contents' -> decode_potion_contents(Data);

		%% Suspicious stew
		suspicious_stew_effects -> decode_suspicious_stew_effects(Data);
		'minecraft:suspicious_stew_effects' -> decode_suspicious_stew_effects(Data);

		%% Book content
		writable_book_content -> decode_writable_book_content(Data);
		'minecraft:writable_book_content' -> decode_writable_book_content(Data);
		written_book_content -> decode_written_book_content(Data);
		'minecraft:written_book_content' -> decode_written_book_content(Data);

		%% Entity data
		entity_data -> decode_entity_data(Data);
		'minecraft:entity_data' -> decode_entity_data(Data);
		block_entity_data -> decode_block_entity_data(Data);
		'minecraft:block_entity_data' -> decode_block_entity_data(Data);

		%% Trim material provider
		provides_trim_material -> decode_provides_trim_material(Data);
		'minecraft:provides_trim_material' -> decode_provides_trim_material(Data);

		%% Fireworks
		fireworks -> decode_fireworks(Data);
		'minecraft:fireworks' -> decode_fireworks(Data);

		%% Profile
		profile_component -> decode_profile_component(Data);
		'minecraft:profile' -> decode_profile_component(Data);

		%% Lodestone tracker
		lodestone_tracker -> decode_lodestone_tracker(Data);
		'minecraft:lodestone_tracker' -> decode_lodestone_tracker(Data);

		%% Pot decorations / block state / bees
		pot_decorations -> decode_pot_decorations(Data);
		'minecraft:pot_decorations' -> decode_pot_decorations(Data);
		block_state -> decode_block_state(Data);
		'minecraft:block_state' -> decode_block_state(Data);
		bees -> decode_bees(Data);
		'minecraft:bees' -> decode_bees(Data);

		%% Break sound
		break_sound -> decode_break_sound(Data);
		'minecraft:break_sound' -> decode_break_sound(Data);

		%% Variant components (simple varint wrappers)
		'minecraft:villager/variant' -> decode_variant_component('minecraft:villager/variant', Data);
		'minecraft:wolf/variant' -> decode_variant_component('minecraft:wolf/variant', Data);
		'minecraft:wolf/sound_variant' -> decode_variant_component('minecraft:wolf/sound_variant', Data);
		'minecraft:fox/variant' -> decode_variant_component('minecraft:fox/variant', Data);
		'minecraft:salmon/size' -> decode_variant_component('minecraft:salmon/size', Data);
		'minecraft:parrot/variant' -> decode_variant_component('minecraft:parrot/variant', Data);
		'minecraft:tropical_fish/pattern' -> decode_variant_component('minecraft:tropical_fish/pattern', Data);
		'minecraft:mooshroom/variant' -> decode_variant_component('minecraft:mooshroom/variant', Data);
		'minecraft:rabbit/variant' -> decode_variant_component('minecraft:rabbit/variant', Data);
		'minecraft:pig/variant' -> decode_variant_component('minecraft:pig/variant', Data);
		'minecraft:pig/sound_variant' -> decode_variant_component('minecraft:pig/sound_variant', Data);
		'minecraft:cow/variant' -> decode_variant_component('minecraft:cow/variant', Data);
		'minecraft:cow/sound_variant' -> decode_variant_component('minecraft:cow/sound_variant', Data);
		'minecraft:chicken/variant' -> decode_variant_component('minecraft:chicken/variant', Data);
		'minecraft:chicken/sound_variant' -> decode_variant_component('minecraft:chicken/sound_variant', Data);
		'minecraft:zombie_nautilus/variant' -> decode_variant_component('minecraft:zombie_nautilus/variant', Data);
		'minecraft:frog/variant' -> decode_variant_component('minecraft:frog/variant', Data);
		'minecraft:horse/variant' -> decode_variant_component('minecraft:horse/variant', Data);
		'minecraft:llama/variant' -> decode_variant_component('minecraft:llama/variant', Data);
		'minecraft:axolotl/variant' -> decode_variant_component('minecraft:axolotl/variant', Data);
		'minecraft:cat/variant' -> decode_variant_component('minecraft:cat/variant', Data);
		'minecraft:cat/sound_variant' -> decode_variant_component('minecraft:cat/sound_variant', Data);

		_ ->
			error({unimplemented_component_decoder, TypeId})
    end.

dye_colors() ->
    [white, orange, magenta, light_blue, yellow, lime, pink, gray,
     light_gray, cyan, purple, blue, brown, green, red, black].

decode_dye_color(BinData) ->
    case decode:decode_type(BinData, {enum, varint, dye_colors()}) of
        {error, Reason} ->
            {error, Reason};
        {Data2, #enum{enum = Colour}} ->
            {Data2, #dye{type = 'minecraft:dye', colour = Colour}}
    end.

decode_trim(Data) ->
	{Data2, TrimMaterial} = decode:decode_type(Data, {id_or_x, trim_material}),
	{Data3, TrimPattern} = decode:decode_type(Data2, {id_or_x, trim_pattern}),
    {Data3, #trim{type = 'minecraft:trim', material = TrimMaterial, pattern = TrimPattern}}.

decode_trim_pattern(Data) ->
    {Data2, #string{string = AssetName}} = decode:decode_type(Data, string),
	{Data3, #varint{varint = TemplateItem}} = decode:decode_type(Data2, varint),
	{Data4, Description} = decode:decode_type(Data3, text_component),
	{Data5, #bool{bool = Decal}} = decode:decode_type(Data4, bool),
    {Data5, #trim_pattern{type = 'minecraft:trim_pattern', asset_name = AssetName, template_item = TemplateItem, description = Description, is_decal = Decal}}.

decode_trim_material(Data) ->
    {Data2, #string{string = Suffix}} = decode:decode_type(Data, string),
    {Data3, #prefixed_array{prefixed_array = Overrides}} = decode:decode_type(Data2, {prefixed_array, [identifier, string]}),
	{Data4, Description} = decode:decode_type(Data3, text_component),
    {Data4, #trim_material{type = 'minecraft:trim_material', suffix = Suffix, overrides = Overrides, description = Description}}.

decode_painting_variant(Data) ->
    {Rest1, #varint{varint = Tag}} = decode:decode_type(Data, varint),
    case Tag of
        0 ->
            {Rest2, #varint{varint = Id}} = decode:decode_type(Rest1, varint),
            {Rest2, #id_or_x{id_or_x = Id}};
        1 ->
            {Rest2, #string{string = AssetId}} = decode:decode_type(Rest1, string),
            {Rest3, #varint{varint = Width}} = decode:decode_type(Rest2, varint),
            {Rest4, #varint{varint = Height}} = decode:decode_type(Rest3, varint),
            {Rest5, #string{string = Title}} = decode:decode_type(Rest4, string),
            {Rest6, #string{string = Author}} = decode:decode_type(Rest5, string),

            VariantMap = #{asset_id => AssetId, width => Width, height => Height, title => Title, author => Author},
            {Rest6, #id_or_x{id_or_x = VariantMap}}
    end.

decode_block_predicate(Data) ->
	{Data2, Blocks} = decode:decode_type(Data, {prefixed_optional, id_set}),
	{Data3, Properties} = decode:decode_type(Data2, {prefixed_optional, {prefixed_array, property}}),
	{Data4, NBT} = decode:decode_type(Data3, {prefixed_optional, nbt}),
	{Data5, #prefixed_array{prefixed_array = DataComponents}} = decode:decode_type(Data4, {prefixed_array, exact_data_component_matcher}),
	{Data6, #prefixed_array{prefixed_array = PartialDataComponents}} = decode:decode_type(Data5, {prefixed_array, partial_data_component_matcher}),
	{Data6, #block_predicate{
		type = 'minecraft:block_predicate',
		blocks = Blocks,
		properties = Properties,
		nbt = NBT,
		data_components = DataComponents,
		partial_data_components = PartialDataComponents
	}}.

decode_property(Data) ->
	{Data2, #string{string = Name}} = decode:decode_type(Data, string),
	{Data3, #bool{bool = IsExactMatch}} = decode:decode_type(Data2, bool),
	case IsExactMatch of
		true ->
			{Data4, #string{string = ExactValue}} = decode:decode_type(Data3, string),
			{Data4, #{name => Name, is_exact_match => true, exact_value => ExactValue}};
		false ->
			{Data4, #string{string = MinValue}} = decode:decode_type(Data3, string),
			{Data5, #string{string = MaxValue}} = decode:decode_type(Data4, string),
			{Data5, #{name => Name, is_exact_match => false, min_value => MinValue, max_value => MaxValue}}
	end.

decode_exact_data_component_matcher(Data) ->
	{Data2, #varint{varint = TypeId}} = decode:decode_type(Data, varint),
	{Data3, Value} = decode_component(TypeId, Data2),
	{Data3, #{type => TypeId, value => Value}}.

decode_partial_data_component_matcher(Data) ->
	{Data2, #varint{varint = TypeId}} = decode:decode_type(Data, varint),
	{Data3, Predicate} = decode:decode_type(Data2, nbt),
	TypeAtom = try data_component_predicate_type_type_registry:id_to_name(TypeId)
	catch _:_ -> TypeId
	end,
	{Data3, #{type => TypeAtom, predicate => Predicate}}.

firework_explosion_shapes() ->
    [small_ball, large_ball, star, creeper, burst].

decode_firework_explosion(Data) ->
    {Data2, #enum{enum = Shape}} = decode:decode_type(Data, {enum, varint, firework_explosion_shapes()}),
    {Data3, #prefixed_array{prefixed_array = ColorsRec}} = decode:decode_type(Data2, {prefixed_array, int}),
    Colors = [case C of #int{int = V} -> V; V -> V end || C <- ColorsRec],
    {Data4, #prefixed_array{prefixed_array = FadeColorsRec}} = decode:decode_type(Data3, {prefixed_array, int}),
    FadeColors = [case C of #int{int = V} -> V; V -> V end || C <- FadeColorsRec],
    {Data5, #bool{bool = HasTrail}} = decode:decode_type(Data4, bool),
    {Data6, #bool{bool = HasTwinkle}} = decode:decode_type(Data5, bool),
    {Data6, #firework_explosion{type = 'minecraft:firework_explosion', shape = Shape, colors = Colors, fade_colors = FadeColors,
        has_trail = HasTrail, has_twinkle = HasTwinkle
    }}.

decode_potion_effect(Data) ->
    {Data2, #varint{varint = IdInt}} = decode:decode_type(Data, varint),
    EffectId = try mob_effect_type_registry:id_to_name(IdInt)
               catch _:_ -> IdInt
               end,
    {Data3, Details} = decode_potion_effect_detail(Data2),
    {Data3, #potion_effect{
        type = 'minecraft:potion_effect',
        id = EffectId,
        details = Details
    }}.

decode_potion_effect_detail(Data) ->
    {Data2, #varint{varint = Amplifier}} = decode:decode_type(Data, varint),
    {Data3, #varint{varint = Duration}} = decode:decode_type(Data2, varint),
    {Data4, #bool{bool = Ambient}} = decode:decode_type(Data3, bool),
    {Data5, #bool{bool = ShowParticles}} = decode:decode_type(Data4, bool),
    {Data6, #bool{bool = ShowIcon}} = decode:decode_type(Data5, bool),
    {Data7, HiddenEffect} = decode:decode_type(Data6, {prefixed_optional, potion_effect_detail}),
    {Data7, #potion_effect_detail{type = 'minecraft:potion_effect_detail', amplifier = Amplifier, duration = Duration,
        ambient = Ambient, show_particles = ShowParticles,show_icon = ShowIcon, hidden_effect = HiddenEffect}}.

decode_consume_effect(Data) ->
    {Data2, #varint{varint = TypeIdInt}} = decode:decode_type(Data, varint),
    EffectType = try consume_effect_type_type_registry:id_to_name(TypeIdInt)
                 catch _:_ -> TypeIdInt
                 end,
    decode_consume_effect_variant(EffectType, Data2).

decode_consume_effect_variant(EffectType, Data) ->
	TypeId = case is_atom(EffectType) of
		true -> consume_effect_type_type_registry:name_to_id(EffectType);
		false -> EffectType
	end,
    case TypeId of
        0 -> decode_apply_effects(Data);
        1 -> decode_remove_effects(Data);
        2 -> decode_clear_all_effects(Data);
        3 -> decode_teleport_randomly(Data);
        4 -> decode_play_sound(Data);
        _ -> error({unknown_consume_effect_type, EffectType})
    end.

decode_apply_effects(Data) ->
    {Data2, #prefixed_array{prefixed_array = Effects}} = decode:decode_type(Data, {prefixed_array, potion_effect}),
    {Data3, #float{float = Probability}} = decode:decode_type(Data2, float),
    {Data3, #consume_effect{type = 'minecraft:consume_effect', effect_type = 'minecraft:apply_effects', effects = Effects, probability = Probability}}.

decode_remove_effects(Data) ->
    {Data2, Effects} = decode:decode_type(Data, id_set),
    {Data2, #consume_effect{type = 'minecraft:consume_effect', effect_type = 'minecraft:remove_effects', effects = Effects}}.

decode_clear_all_effects(Data) ->
    {Data, #consume_effect{type = 'minecraft:consume_effect', effect_type = 'minecraft:clear_all_effects'}}.

decode_teleport_randomly(Data) ->
    {Data2, #float{float = Diameter}} = decode:decode_type(Data, float),
    {Data2, #consume_effect{type = 'minecraft:consume_effect', effect_type = 'minecraft:teleport_randomly', diameter = Diameter}}.

decode_play_sound(Data) ->
    {Data2, Sound} = decode:decode_type(Data, sound_event),
    {Data2, #consume_effect{type = 'minecraft:consume_effect', effect_type = 'minecraft:play_sound', sound = Sound}}.

decode_instrument(Data) ->
    {Data2, SoundEvent} = decode:decode_type(Data, {id_or_x, sound_event}),
    {Data3, #float{float = UseDuration}} = decode:decode_type(Data2, float),
    {Data4, #float{float = Range}} = decode:decode_type(Data3, float),
    {Data5, Description} = decode:decode_type(Data4, text_component),
    {Data5, #instrument{type = 'minecraft:instrument', sound_event = SoundEvent, use_duration = UseDuration, range = Range, description = Description}}.

decode_jukebox_song(Data) ->
    {Data2, SoundEvent} = decode:decode_type(Data, {id_or_x, jukebox_song}),
    {Data3, Description} = decode:decode_type(Data2, text_component),
    {Data4, #float{float = Duration}} = decode:decode_type(Data3, float),
    {Data5, #varint{varint = Output}} = decode:decode_type(Data4, varint),
    {Data5, #jukebox_song{type = 'minecraft:jukebox_song', sound_event = SoundEvent, description = Description, duration = Duration, output = Output}}.

decode_banner_pattern(Data) ->
    {Data2, AssetId} = decode:decode_type(Data, identifier),
    {Data3, #string{string = TranslationKey}} = decode:decode_type(Data2, string),
    {Data3, #banner_pattern{type = 'minecraft:banner_pattern', asset_id = AssetId, translation_key = TranslationKey}}.

decode_nbt_raw(Data) ->
    case nbt:decode(Data) of
        {Rest, Map} -> {Rest, Map};
        Map when is_list(Map) -> {<<>>, Map}
    end.

decode_custom_data(Data) ->
    {Rest, NbtData} = decode_nbt_raw(Data),
    {Rest, #custom_data{type = 'minecraft:custom_data', data = NbtData}}.

decode_max_stack_size(Data) ->
    {Data2, #varint{varint = MaxStackSize}} = decode:decode_type(Data, varint),
    {Data2, #max_stack_size{type = 'minecraft:max_stack_size', max_stack_size = MaxStackSize}}.

decode_max_damage(Data) ->
    {Data2, #varint{varint = MaxDamage}} = decode:decode_type(Data, varint),
    {Data2, #max_damage{type = 'minecraft:max_damage', max_damage = MaxDamage}}.

decode_damage(Data) ->
    {Data2, #varint{varint = Damage}} = decode:decode_type(Data, varint),
    {Data2, #damage{type = 'minecraft:damage', damage = Damage}}.

decode_unbreakable(Data) ->
    {Data, #unbreakable{type = 'minecraft:unbreakable'}}.


decode_custom_name(Data) ->
    {Data2, Name} = decode:decode_type(Data, text_component),
    {Data2, #custom_name{type = 'minecraft:custom_name', name = Name}}.

decode_item_name(Data) ->
    {Data2, Name} = decode:decode_type(Data, text_component),
    {Data2, #item_name{type = 'minecraft:item_name', name = Name}}.

decode_item_model(Data) ->
    {Data2, Model} = decode:decode_type(Data, identifier),
    {Data2, #item_model{type = 'minecraft:item_model', model = Model}}.

decode_lore(Data) ->
    {Data2, #prefixed_array{prefixed_array = Lines}} = decode:decode_type(Data, {prefixed_array, text_component}),
    {Data2, #lore{type = 'minecraft:lore', lines = Lines}}.

rarity_values() -> [common, uncommon, rare, epic].

decode_rarity(Data) ->
    {Data2, #enum{enum = Rarity}} = decode:decode_type(Data, {enum, varint, rarity_values()}),
    {Data2, #rarity{type = 'minecraft:rarity', rarity = Rarity}}.

decode_tooltip_style(Data) ->
    {Data2, Style} = decode:decode_type(Data, identifier),
    {Data2, #tooltip_style{type = 'minecraft:tooltip_style', style = Style}}.

decode_note_block_sound(Data) ->
    {Data2, Sound} = decode:decode_type(Data, identifier),
    {Data2, #note_block_sound{type = 'minecraft:note_block_sound', sound = Sound}}.

decode_repair_cost(Data) ->
    {Data2, #varint{varint = Cost}} = decode:decode_type(Data, varint),
    {Data2, #repair_cost{type = 'minecraft:repair_cost', cost = Cost}}.

decode_creative_slot_lock(Data) ->
    {Data, #creative_slot_lock{type = 'minecraft:creative_slot_lock'}}.

decode_enchantment_glint_override(Data) ->
    {Data2, #bool{bool = HasGlint}} = decode:decode_type(Data, bool),
    {Data2, #enchantment_glint_override{type = 'minecraft:enchantment_glint_override', has_glint = HasGlint}}.

decode_glider(Data) ->
    {Data, #glider{type = 'minecraft:glider'}}.

decode_enchantable(Data) ->
    {Data2, #varint{varint = Value}} = decode:decode_type(Data, varint),
    {Data2, #enchantable{type = 'minecraft:enchantable', value = Value}}.

decode_map_id(Data) ->
    {Data2, #varint{varint = Id}} = decode:decode_type(Data, varint),
    {Data2, #map_id{type = 'minecraft:map_id', id = Id}}.

decode_ominous_bottle_amplifier(Data) ->
    {Data2, #varint{varint = Amplifier}} = decode:decode_type(Data, varint),
    {Data2, #ominous_bottle_amplifier{type = 'minecraft:ominous_bottle_amplifier', amplifier = Amplifier}}.

decode_potion_duration_scale(Data) ->
    {Data2, #float{float = Scale}} = decode:decode_type(Data, float),
    {Data2, #potion_duration_scale{type = 'minecraft:potion_duration_scale', scale = Scale}}.

decode_additional_trade_cost(Data) ->
    {Data2, #varint{varint = Cost}} = decode:decode_type(Data, varint),
    {Data2, #additional_trade_cost{type = 'minecraft:additional_trade_cost', cost = Cost}}.

decode_minimum_attack_charge(Data) ->
    {Data2, #float{float = Charge}} = decode:decode_type(Data, float),
    {Data2, #minimum_attack_charge{type = 'minecraft:minimum_attack_charge', charge = Charge}}.

decode_intangible_projectile(Data) ->
    {Rest, NbtData} = decode_nbt_raw(Data),
    {Rest, #intangible_projectile{type = 'minecraft:intangible_projectile', data = NbtData}}.

decode_map_decorations(Data) ->
    {Rest, NbtData} = decode_nbt_raw(Data),
    {Rest, #map_decorations{type = 'minecraft:map_decorations', data = NbtData}}.

decode_debug_stick_state(Data) ->
    {Rest, NbtData} = decode_nbt_raw(Data),
    {Rest, #debug_stick_state{type = 'minecraft:debug_stick_state', data = NbtData}}.

decode_bucket_entity_data(Data) ->
    {Rest, NbtData} = decode_nbt_raw(Data),
    {Rest, #bucket_entity_data{type = 'minecraft:bucket_entity_data', data = NbtData}}.

decode_lock(Data) ->
    {Rest, NbtData} = decode_nbt_raw(Data),
    {Rest, #lock{type = 'minecraft:lock', key = NbtData}}.

decode_container_loot(Data) ->
    {Rest, NbtData} = decode_nbt_raw(Data),
    {Rest, #container_loot{type = 'minecraft:container_loot', data = NbtData}}.

decode_recipes(Data) ->
    {Rest, NbtData} = decode_nbt_raw(Data),
    {Rest, #recipes{type = 'minecraft:recipes', data = NbtData}}.

decode_dyed_color(Data) ->
    {Data2, #int{int = Color}} = decode:decode_type(Data, int),
    {Data2, #dyed_color{type = 'minecraft:dyed_color', color = Color}}.

decode_map_color(Data) ->
    {Data2, #int{int = Color}} = decode:decode_type(Data, int),
    {Data2, #map_color{type = 'minecraft:map_color', color = Color}}.

map_post_processing_types() -> [lock, scale].

decode_map_post_processing(Data) ->
    {Data2, #enum{enum = Type}} = decode:decode_type(Data, {enum, varint, map_post_processing_types()}),
    {Data2, #map_post_processing{type = 'minecraft:map_post_processing', processing_type = Type}}.

decode_base_color(Data) ->
    case decode:decode_type(Data, {enum, varint, dye_colors()}) of
        {error, Reason} -> {error, Reason};
        {Data2, #enum{enum = Color}} ->
            {Data2, #base_color{type = 'minecraft:base_color', color = Color}}
    end.

decode_charged_projectiles(Data) ->
    {Data2, #prefixed_array{prefixed_array = Projectiles}} = decode:decode_type(Data, {prefixed_array, slot}),
    {Data2, #charged_projectiles{type = 'minecraft:charged_projectiles', projectiles = Projectiles}}.

decode_bundle_contents(Data) ->
    {Data2, #prefixed_array{prefixed_array = Items}} = decode:decode_type(Data, {prefixed_array, slot}),
    {Data2, #bundle_contents{type = 'minecraft:bundle_contents', items = Items}}.

decode_use_remainder(Data) ->
    {Data2, Remainder} = decode:decode_type(Data, slot),
    {Data2, #use_remainder{type = 'minecraft:use_remainder', remainder = Remainder}}.

decode_container(Data) ->
    {Data2, #prefixed_array{prefixed_array = Items}} = decode:decode_type(Data, {prefixed_array, slot}),
    {Data2, #container{type = 'minecraft:container', items = Items}}.

decode_sulfur_cube_content(Data) ->
    {Data2, Content} = decode:decode_type(Data, slot),
    {Data2, #sulfur_cube_content{type = 'minecraft:sulfur_cube_content', content = Content}}.

decode_enchantments(Data) ->
    {Data2, #prefixed_array{prefixed_array = Enchants}} = decode:decode_type(Data, {prefixed_array, [varint, varint]}),
    {Data2, #enchantments{type = 'minecraft:enchantments', enchantments = Enchants}}.

decode_stored_enchantments(Data) ->
    {Data2, #prefixed_array{prefixed_array = Enchants}} = decode:decode_type(Data, {prefixed_array, [varint, varint]}),
    {Data2, #stored_enchantments{type = 'minecraft:stored_enchantments', enchantments = Enchants}}.

attribute_operations() -> [add, multiply_base, multiply_total].
attribute_slots() -> [any, main_hand, off_hand, hand, feet, legs, chest, head, armor, body].

decode_attribute_modifiers(Data) ->
    {Data2, #prefixed_array{prefixed_array = Modifiers}} = decode:decode_type(Data, {prefixed_array, [varint, identifier, double, {enum, varint, attribute_operations()}, {enum, varint, attribute_slots()}]}),
    {Data2, #attribute_modifiers{type = 'minecraft:attribute_modifiers', modifiers = Modifiers}}.

decode_custom_model_data(Data) ->
    {Data2, #prefixed_array{prefixed_array = Floats}} = decode:decode_type(Data, {prefixed_array, float}),
    {Data3, #prefixed_array{prefixed_array = Flags}} = decode:decode_type(Data2, {prefixed_array, bool}),
    {Data4, #prefixed_array{prefixed_array = Strings}} = decode:decode_type(Data3, {prefixed_array, string}),
    {Data5, #prefixed_array{prefixed_array = Colors}} = decode:decode_type(Data4, {prefixed_array, int}),
    {Data5, #custom_model_data{type = 'minecraft:custom_model_data', floats = Floats, flags = Flags, strings = Strings, colors = Colors}}.

decode_tooltip_display(Data) ->
    {Data2, #bool{bool = HideTooltip}} = decode:decode_type(Data, bool),
    {Data3, #prefixed_array{prefixed_array = HiddenComponents}} = decode:decode_type(Data2, {prefixed_array, varint}),
    {Data3, #tooltip_display{type = 'minecraft:tooltip_display', hide_tooltip = HideTooltip, hidden_components = HiddenComponents}}.

decode_food(Data) ->
    {Data2, #varint{varint = Nutrition}} = decode:decode_type(Data, varint),
    {Data3, #float{float = Saturation}} = decode:decode_type(Data2, float),
    {Data4, #bool{bool = CanAlwaysEat}} = decode:decode_type(Data3, bool),
    {Data4, #food{type = 'minecraft:food', nutrition = Nutrition, saturation_modifier = Saturation, can_always_eat = CanAlwaysEat}}.

consumable_animations() -> [none, eat, drink, block, bow, spear, crossbow, spyglass, toot_horn, brush].

decode_consumable(Data) ->
    {Data2, #float{float = ConsumeSeconds}} = decode:decode_type(Data, float),
    {Data3, #enum{enum = Animation}} = decode:decode_type(Data2, {enum, varint, consumable_animations()}),
    {Data4, Sound} = decode:decode_type(Data3, {id_or_x, sound_event}),
    {Data5, #bool{bool = HasParticles}} = decode:decode_type(Data4, bool),
    {Data6, #prefixed_array{prefixed_array = Effects}} = decode:decode_type(Data5, {prefixed_array, consume_effect}),
    {Data6, #consumable{type = 'minecraft:consumable', consume_seconds = ConsumeSeconds, animation = Animation,
        sound = Sound, has_consume_particles = HasParticles, effects = Effects}}.

decode_use_cooldown(Data) ->
    {Data2, #float{float = Seconds}} = decode:decode_type(Data, float),
    {Data3, CooldownGroup} = decode:decode_type(Data2, {prefixed_optional, identifier}),
    {Data3, #use_cooldown{type = 'minecraft:use_cooldown', seconds = Seconds, cooldown_group = CooldownGroup}}.

decode_use_effects(Data) ->
    {Data2, #bool{bool = CanSprint}} = decode:decode_type(Data, bool),
    {Data3, #bool{bool = InteractVibrations}} = decode:decode_type(Data2, bool),
    {Data4, #float{float = SpeedMultiplier}} = decode:decode_type(Data3, float),
    {Data4, #use_effects{type = 'minecraft:use_effects', can_sprint = CanSprint, interact_vibrations = InteractVibrations, speed_multiplier = SpeedMultiplier}}.

decode_damage_type(Data) ->
    {Data2, #varint{varint = DamageType}} = decode:decode_type(Data, varint),
    {Data2, #damage_type{type = 'minecraft:damage_type', damage_type = DamageType}}.

decode_damage_resistant(Data) ->
    {Data2, Types} = decode:decode_type(Data, id_set),
    {Data2, #damage_resistant{type = 'minecraft:damage_resistant', types = Types}}.

decode_attack_range(Data) ->
    {Data2, #float{float = MinReach}} = decode:decode_type(Data, float),
    {Data3, #float{float = MaxReach}} = decode:decode_type(Data2, float),
    {Data4, #float{float = MinCreativeReach}} = decode:decode_type(Data3, float),
    {Data5, #float{float = MaxCreativeReach}} = decode:decode_type(Data4, float),
    {Data6, #float{float = HitboxMargin}} = decode:decode_type(Data5, float),
    {Data7, #float{float = MobFactor}} = decode:decode_type(Data6, float),
    {Data7, #attack_range{type = 'minecraft:attack_range', min_reach = MinReach, max_reach = MaxReach,
        min_creative_reach = MinCreativeReach, max_creative_reach = MaxCreativeReach,
        hitbox_margin = HitboxMargin, mob_factor = MobFactor}}.

decode_weapon(Data) ->
    {Data2, #varint{varint = DamagePerAttack}} = decode:decode_type(Data, varint),
    {Data3, #float{float = DisableBlockingFor}} = decode:decode_type(Data2, float),
    {Data3, #weapon{type = 'minecraft:weapon', damage_per_attack = DamagePerAttack, disable_blocking_for = DisableBlockingFor}}.

decode_piercing_weapon(Data) ->
    {Data2, #bool{bool = DealsKnockback}} = decode:decode_type(Data, bool),
    {Data3, #bool{bool = Dismounts}} = decode:decode_type(Data2, bool),
    {Data4, Sound} = decode:decode_type(Data3, {prefixed_optional, sound_event}),
    {Data5, HitSound} = decode:decode_type(Data4, {prefixed_optional, sound_event}),
    {Data5, #piercing_weapon{type = 'minecraft:piercing_weapon', deals_knockback = DealsKnockback,
        dismounts = Dismounts, sound = Sound, hit_sound = HitSound}}.

decode_kinetic_weapon(Data) ->
    {Data2, #varint{varint = ContactCooldownTicks}} = decode:decode_type(Data, varint),
    {Data3, #varint{varint = DelayTicks}} = decode:decode_type(Data2, varint),
    {Data4, DismountConditions} = decode:decode_type(Data3, {prefixed_optional, nbt}),
    {Data5, KnockbackConditions} = decode:decode_type(Data4, {prefixed_optional, nbt}),
    {Data6, DamageConditions} = decode:decode_type(Data5, {prefixed_optional, nbt}),
    {Data7, #float{float = ForwardMovement}} = decode:decode_type(Data6, float),
    {Data8, #float{float = DamageMultiplier}} = decode:decode_type(Data7, float),
    {Data9, Sound} = decode:decode_type(Data8, {prefixed_optional, sound_event}),
    {Data10, HitSound} = decode:decode_type(Data9, {prefixed_optional, sound_event}),
    {Data10, #kinetic_weapon{type = 'minecraft:kinetic_weapon', contact_cooldown_ticks = ContactCooldownTicks,
        delay_ticks = DelayTicks, dismount_conditions = DismountConditions, knockback_conditions = KnockbackConditions,
        damage_conditions = DamageConditions, forward_movement = ForwardMovement, damage_multiplier = DamageMultiplier,
        sound = Sound, hit_sound = HitSound}}.

swing_animation_types() -> [none, whack, stab].

decode_swing_animation(Data) ->
    {Data2, #enum{enum = AnimationType}} = decode:decode_type(Data, {enum, varint, swing_animation_types()}),
    {Data3, #varint{varint = Duration}} = decode:decode_type(Data2, varint),
    {Data3, #swing_animation{type = 'minecraft:swing_animation', animation_type = AnimationType, duration = Duration}}.

decode_tool(Data) ->
    {Data2, #prefixed_array{prefixed_array = Rules}} = decode:decode_type(Data, {prefixed_array, [id_set, {prefixed_optional, float}, {prefixed_optional, bool}]}),
    {Data3, #float{float = DefaultMiningSpeed}} = decode:decode_type(Data2, float),
    {Data4, #varint{varint = DamagePerBlock}} = decode:decode_type(Data3, varint),
    {Data5, #bool{bool = CanDestroyBlocksInCreative}} = decode:decode_type(Data4, bool),
    {Data5, #tool{type = 'minecraft:tool', rules = Rules, default_mining_speed = DefaultMiningSpeed,
        damage_per_block = DamagePerBlock, can_destroy_blocks_in_creative = CanDestroyBlocksInCreative}}.

equippable_slots() -> [mainhand, feet, legs, chest, head, offhand, body].

decode_equippable(Data) ->
    {Data2, #enum{enum = Slot}} = decode:decode_type(Data, {enum, varint, equippable_slots()}),
    {Data3, EquipSound} = decode:decode_type(Data2, {id_or_x, sound_event}),
    {Data4, Model} = decode:decode_type(Data3, {prefixed_optional, identifier}),
    {Data5, CameraOverlay} = decode:decode_type(Data4, {prefixed_optional, identifier}),
    {Data6, AllowedEntities} = decode:decode_type(Data5, {prefixed_optional, id_set}),
    {Data7, #bool{bool = Dispensable}} = decode:decode_type(Data6, bool),
    {Data8, #bool{bool = Swappable}} = decode:decode_type(Data7, bool),
    {Data9, #bool{bool = DamageOnHurt}} = decode:decode_type(Data8, bool),
    {Data10, #bool{bool = CanBeSheared}} = decode:decode_type(Data9, bool),
    {Data11, ShearingSound} = decode:decode_type(Data10, {id_or_x, sound_event}),
    {Data11, #equippable{type = 'minecraft:equippable', slot = Slot, equip_sound = EquipSound,
        model = Model, camera_overlay = CameraOverlay, allowed_entities = AllowedEntities,
        dispensable = Dispensable, swappable = Swappable, damage_on_hurt = DamageOnHurt,
        can_be_sheared = CanBeSheared, shearing_sound = ShearingSound}}.

decode_repairable(Data) ->
    {Data2, Items} = decode:decode_type(Data, id_set),
    {Data2, #repairable{type = 'minecraft:repairable', items = Items}}.

decode_death_protection(Data) ->
    {Data2, #prefixed_array{prefixed_array = Effects}} = decode:decode_type(Data, {prefixed_array, consume_effect}),
    {Data2, #death_protection{type = 'minecraft:death_protection', effects = Effects}}.

decode_blocks_attacks(Data) ->
    {Data2, #float{float = BlockDelaySeconds}} = decode:decode_type(Data, float),
    {Data3, #float{float = DisableCooldownScale}} = decode:decode_type(Data2, float),
    {Data4, #prefixed_array{prefixed_array = DamageReductions}} = decode:decode_type(Data3, {prefixed_array, [float, {prefixed_optional, id_set}, float, float]}),
    {Data5, #float{float = ItemDamageThreshold}} = decode:decode_type(Data4, float),
    {Data6, #float{float = ItemDamageBase}} = decode:decode_type(Data5, float),
    {Data7, #float{float = ItemDamageFactor}} = decode:decode_type(Data6, float),
    {Data8, BypassedBy} = decode:decode_type(Data7, {prefixed_optional, id_set}),
    {Data9, BlockSound} = decode:decode_type(Data8, {prefixed_optional, {id_or_x, sound_event}}),
    {Data10, DisableSound} = decode:decode_type(Data9, {prefixed_optional, {id_or_x, sound_event}}),
    {Data10, #blocks_attacks{type = 'minecraft:blocks_attacks', block_delay_seconds = BlockDelaySeconds,
        disable_cooldown_scale = DisableCooldownScale,
        damage_reductions = #{reductions => DamageReductions, item_damage_threshold => ItemDamageThreshold,
            item_damage_base => ItemDamageBase, item_damage_factor => ItemDamageFactor},
        bypassed_by = BypassedBy, block_sound = BlockSound, disable_sound = DisableSound}}.

decode_potion_contents(Data) ->
    {Data2, PotionId} = decode:decode_type(Data, {prefixed_optional, varint}),
    {Data3, CustomColor} = decode:decode_type(Data2, {prefixed_optional, int}),
    {Data4, #prefixed_array{prefixed_array = CustomEffects}} = decode:decode_type(Data3, {prefixed_array, potion_effect}),
    {Data5, CustomName} = decode:decode_type(Data4, {prefixed_optional, string}),
    {Data5, #potion_contents{type = 'minecraft:potion_contents', potion_id = PotionId,
        custom_color = CustomColor, custom_effects = CustomEffects, custom_name = CustomName}}.

decode_suspicious_stew_effects(Data) ->
    {Data2, #prefixed_array{prefixed_array = Effects}} = decode:decode_type(Data, {prefixed_array, [varint, varint]}),
    {Data2, #suspicious_stew_effects{type = 'minecraft:suspicious_stew_effects', effects = Effects}}.

decode_writable_book_content(Data) ->
    {Data2, #prefixed_array{prefixed_array = Pages}} = decode:decode_type(Data, {prefixed_array, [string, {prefixed_optional, string}]}),
    {Data2, #writable_book_content{type = 'minecraft:writable_book_content', pages = Pages}}.

decode_written_book_content(Data) ->
    {Data2, #string{string = RawTitle}} = decode:decode_type(Data, string),
    {Data3, FilteredTitle} = decode:decode_type(Data2, {prefixed_optional, string}),
    {Data4, #string{string = Author}} = decode:decode_type(Data3, string),
    {Data5, #varint{varint = Generation}} = decode:decode_type(Data4, varint),
    {Data6, #prefixed_array{prefixed_array = Pages}} = decode:decode_type(Data5, {prefixed_array, [text_component, {prefixed_optional, text_component}]}),
    {Data7, #bool{bool = Resolved}} = decode:decode_type(Data6, bool),
    {Data7, #written_book_content{type = 'minecraft:written_book_content', raw_title = RawTitle,
        filtered_title = FilteredTitle, author = Author, generation = Generation,
        pages = Pages, resolved = Resolved}}.

decode_entity_data(Data) ->
    {Data2, #varint{varint = EntityType}} = decode:decode_type(Data, varint),
    {Data3, NbtData} = decode_nbt_raw(Data2),
    {Data3, #entity_data{type = 'minecraft:entity_data', entity_type = EntityType, data = NbtData}}.

decode_block_entity_data(Data) ->
    {Data2, #varint{varint = BlockEntityType}} = decode:decode_type(Data, varint),
    {Data3, NbtData} = decode_nbt_raw(Data2),
    {Data3, #block_entity_data{type = 'minecraft:block_entity_data', block_entity_type = BlockEntityType, data = NbtData}}.

decode_provides_trim_material(Data) ->
    {Data2, Key} = decode:decode_type(Data, id_set),
    {Data2, #provides_trim_material{type = 'minecraft:provides_trim_material', key = Key}}.

decode_fireworks(Data) ->
    {Data2, #varint{varint = FlightDuration}} = decode:decode_type(Data, varint),
    {Data3, #prefixed_array{prefixed_array = Explosions}} = decode:decode_type(Data2, {prefixed_array, firework_explosion}),
    {Data3, #fireworks{type = 'minecraft:fireworks', flight_duration = FlightDuration, explosions = Explosions}}.

decode_profile_component(Data) ->
    {Data2, Profile} = decode:decode_type(Data, {resolvable_profile, false, false, false, false}),
    {Data2, #profile_component{type = 'minecraft:profile', profile = Profile}}.

decode_lodestone_tracker(Data) ->
    {Data2, #bool{bool = HasGlobalPosition}} = decode:decode_type(Data, bool),
    case HasGlobalPosition of
        true ->
            {Data3, Dimension} = decode:decode_type(Data2, identifier),
            {Data4, Position} = decode:decode_type(Data3, position),
            {Data5, #bool{bool = Tracked}} = decode:decode_type(Data4, bool),
            {Data5, #lodestone_tracker{type = 'minecraft:lodestone_tracker', has_global_position = true,
                dimension = Dimension, position = Position, tracked = Tracked}};
        false ->
            {Data3, #bool{bool = Tracked}} = decode:decode_type(Data2, bool),
            {Data3, #lodestone_tracker{type = 'minecraft:lodestone_tracker', has_global_position = false,
                dimension = undefined, position = undefined, tracked = Tracked}}
    end.

decode_pot_decorations(Data) ->
    {Data2, #prefixed_array{prefixed_array = Decorations}} = decode:decode_type(Data, {prefixed_array, varint}),
    {Data2, #pot_decorations{type = 'minecraft:pot_decorations', decorations = Decorations}}.

decode_block_state(Data) ->
    {Data2, #prefixed_array{prefixed_array = Properties}} = decode:decode_type(Data, {prefixed_array, [string, string]}),
    {Data2, #block_state{type = 'minecraft:block_state', properties = Properties}}.

decode_bees(Data) ->
    {Data2, #prefixed_array{prefixed_array = BeesData}} = decode:decode_type(Data, {prefixed_array, [varint, nbt, varint, varint]}),
    {Data2, #bees{type = 'minecraft:bees', bees = BeesData}}.

decode_break_sound(Data) ->
    {Data2, SoundEvent} = decode:decode_type(Data, {id_or_x, sound_event}),
    {Data2, #break_sound{type = 'minecraft:break_sound', sound_event = SoundEvent}}.

decode_variant_component(TypeAtom, Data) ->
    {Data2, #varint{varint = Variant}} = decode:decode_type(Data, varint),
    {Data2, #variant_component{type = TypeAtom, variant = Variant}}.
