-module(component_decode).

-export([
	 decode_component/2
	]).


-include("src/data_types/components/component_records.hrl").



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
        {Data2, Colour} ->
            {Data2, #dye{type = 'minecraft:dye', colour = Colour}}
    end.

decode_trim(Data) ->
	{Data2, TrimMaterial} = decode:decode_type(Data, {id_or_x, trim_material}),
	{Data3, TrimPattern} = decode:decode_type(Data2, {id_or_x, trim_pattern}),
    {Data3, #trim{type = 'minecraft:trim', material = TrimMaterial, pattern = TrimPattern}}.


decode_trim_pattern(Data) ->
    {Data2, AssetName} = decode:decode_type(Data, string),
	{Data3, TemplateItem} = decode:decode_type(Data2, varint),
	{Data4, Description} = decode:decode_type(Data3, text_component),
	{Data5, Decal} = decode:decode_type(Data4, bool),
    {Data5, #trim_pattern{type = 'minecraft:trim_pattern', asset_name = AssetName, template_item = TemplateItem, description = Description, is_decal = Decal}}.

decode_trim_material(Data) ->
    {Data2, Suffix} = decode:decode_type(Data, string),
    {Data3, Overrides} = decode:decode_type(Data2, {prefixed_array, [identifier, string]}),
	{Data4, Description} = decode:decode_type(Data3, text_component),
    {Data4, #trim_material{type = 'minecraft:trim_material', suffix = Suffix, overrides = Overrides, description = Description}}.


%%Trim Material
%%See also: Minecraft Wiki:Projects/wiki.vg merge/Registry Data § Armor Trim Material
%%Name 	Type 	Description
%%Suffix 	String 	
%%Overrides 	Armor Material Type 	Prefixed Array 	Identifier 	
%%Overriden Asset Name 	String 	
%%Description 	Text Component

decode_painting_variant(Data) ->
    {Rest1, Tag} = decode:decode_type(Data, varint),
    case Tag of
        0 ->
            {Rest2, Id} = decode:decode_type(Rest1, varint),
            {Rest2, {id, Id}};
        1 ->
            {Rest2, AssetId} = decode:decode_type(Rest1, string),
            {Rest3, Width} = decode:decode_type(Rest2, varint),
            {Rest4, Height} = decode:decode_type(Rest3, varint),
            {Rest5, Title} = decode:decode_type(Rest4, string),
            {Rest6, Author} = decode:decode_type(Rest5, string),

            VariantMap = #{asset_id => AssetId, width => Width, height => Height, title => Title, author => Author},
            {Rest6, {inline, VariantMap}}
    end.


decode_block_predicate(Data) ->
	{Data2, Blocks} = decode:decode_type(Data, {prefixed_optional, id_set}),
	{Data3, Properties} = decode:decode_type(Data2, {prefixed_optional, {prefixed_array, property}}),
	{Data4, NBT} = decode:decode_type(Data3, {prefixed_optional, nbt}),
	{Data5, DataComponents} = decode:decode_type(Data4, {prefixed_array, exact_data_component_matcher}),
	{Data6, PartialDataComponents} = decode:decode_type(Data5, {prefixed_array, partial_data_component_matcher}),
	{Data6, #block_predicate{
		type = 'minecraft:block_predicate',
		blocks = Blocks,
		properties = Properties,
		nbt = NBT,
		data_components = DataComponents,
		partial_data_components = PartialDataComponents
	}}.

decode_property(Data) ->
	{Data2, Name} = decode:decode_type(Data, string),
	{Data3, IsExactMatch} = decode:decode_type(Data2, bool),
	case IsExactMatch of
		true ->
			{Data4, ExactValue} = decode:decode_type(Data3, string),
			{Data4, #{name => Name, is_exact_match => true, exact_value => ExactValue}};
		false ->
			{Data4, MinValue} = decode:decode_type(Data3, string),
			{Data5, MaxValue} = decode:decode_type(Data4, string),
			{Data5, #{name => Name, is_exact_match => false, min_value => MinValue, max_value => MaxValue}}
	end.

decode_exact_data_component_matcher(Data) ->
	{Data2, TypeId} = decode:decode_type(Data, varint),
	{Data3, Value} = decode_component(TypeId, Data2),
	{Data3, #{type => TypeId, value => Value}}.

decode_partial_data_component_matcher(Data) ->
	{Data2, TypeId} = decode:decode_type(Data, varint),
	{Data3, Predicate} = decode:decode_type(Data2, nbt),
	TypeAtom = try data_component_predicate_type_type_registry:id_to_name(TypeId)
	catch _:_ -> TypeId
	end,
	{Data3, #{type => TypeAtom, predicate => Predicate}}.

firework_explosion_shapes() ->
    [small_ball, large_ball, star, creeper, burst].

decode_firework_explosion(Data) ->
    {Data2, Shape} = decode:decode_type(Data, {enum, varint, firework_explosion_shapes()}),
    {Data3, Colors} = decode:decode_type(Data2, {prefixed_array, int}),
    {Data4, FadeColors} = decode:decode_type(Data3, {prefixed_array, int}),
    {Data5, HasTrail} = decode:decode_type(Data4, bool),
    {Data6, HasTwinkle} = decode:decode_type(Data5, bool),
    {Data6, #firework_explosion{type = 'minecraft:firework_explosion', shape = Shape, colors = Colors, fade_colors = FadeColors,
        has_trail = HasTrail, has_twinkle = HasTwinkle
    }}.

decode_potion_effect(Data) ->
    {Data2, IdInt} = decode:decode_type(Data, varint),
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
    {Data2, Amplifier} = decode:decode_type(Data, varint),
    {Data3, Duration} = decode:decode_type(Data2, varint),
    {Data4, Ambient} = decode:decode_type(Data3, bool),
    {Data5, ShowParticles} = decode:decode_type(Data4, bool),
    {Data6, ShowIcon} = decode:decode_type(Data5, bool),
    {Data7, HiddenEffect} = decode:decode_type(Data6, {prefixed_optional, potion_effect_detail}),
    {Data7, #potion_effect_detail{type = 'minecraft:potion_effect_detail', amplifier = Amplifier, duration = Duration,
        ambient = Ambient, show_particles = ShowParticles,show_icon = ShowIcon, hidden_effect = HiddenEffect}}.

decode_consume_effect(Data) ->
    {Data2, TypeIdInt} = decode:decode_type(Data, varint),
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
    {Data2, Effects} = decode:decode_type(Data, {prefixed_array, potion_effect}),
    {Data3, Probability} = decode:decode_type(Data2, float),
    {Data3, #consume_effect{type = 'minecraft:consume_effect', effect_type = 'minecraft:apply_effects', effects = Effects, probability = Probability}}.

decode_remove_effects(Data) ->
    {Data2, Effects} = decode:decode_type(Data, id_set),
    {Data2, #consume_effect{type = 'minecraft:consume_effect', effect_type = 'minecraft:remove_effects', effects = Effects}}.

decode_clear_all_effects(Data) ->
    {Data, #consume_effect{type = 'minecraft:consume_effect', effect_type = 'minecraft:clear_all_effects'}}.

decode_teleport_randomly(Data) ->
    {Data2, Diameter} = decode:decode_type(Data, float),
    {Data2, #consume_effect{type = 'minecraft:consume_effect', effect_type = 'minecraft:teleport_randomly', diameter = Diameter}}.

decode_play_sound(Data) ->
    {Data2, Sound} = decode:decode_type(Data, sound_event),
    {Data2, #consume_effect{type = 'minecraft:consume_effect', effect_type = 'minecraft:play_sound', sound = Sound}}.

decode_instrument(Data) ->
    {Data2, SoundEvent} = decode:decode_type(Data, {id_or_x, sound_event}),
    {Data3, UseDuration} = decode:decode_type(Data2, float),
    {Data4, Range} = decode:decode_type(Data3, float),
    {Data5, Description} = decode:decode_type(Data4, text_component),
    {Data5, #instrument{type = 'minecraft:instrument', sound_event = SoundEvent, use_duration = UseDuration, range = Range, description = Description}}.

decode_jukebox_song(Data) ->
    {Data2, SoundEvent} = decode:decode_type(Data, {id_or_x, sound_event}),
    {Data3, Description} = decode:decode_type(Data2, text_component),
    {Data4, Duration} = decode:decode_type(Data3, float),
    {Data5, Output} = decode:decode_type(Data4, varint),
    {Data5, #jukebox_song{type = 'minecraft:jukebox_song', sound_event = SoundEvent, description = Description, duration = Duration, output = Output}}.

decode_banner_pattern(Data) ->
    {Data2, AssetId} = decode:decode_type(Data, identifier),
    {Data3, TranslationKey} = decode:decode_type(Data2, string),
    {Data3, #banner_pattern{type = 'minecraft:banner_pattern', asset_id = AssetId, translation_key = TranslationKey}}.

decode_custom_data(Data) ->
    NbtData = nbt:decode(Data),
    {<<>>, #custom_data{type = 'minecraft:custom_data', data = NbtData}}.

decode_max_stack_size(Data) ->
    {Data2, MaxStackSize} = decode:decode_type(Data, varint),
    {Data2, #max_stack_size{type = 'minecraft:max_stack_size', max_stack_size = MaxStackSize}}.

decode_max_damage(Data) ->
    {Data2, MaxDamage} = decode:decode_type(Data, varint),
    {Data2, #max_damage{type = 'minecraft:max_damage', max_damage = MaxDamage}}.

decode_damage(Data) ->
    {Data2, Damage} = decode:decode_type(Data, varint),
    {Data2, #damage{type = 'minecraft:damage', damage = Damage}}.

decode_unbreakable(Data) ->
    {Data, #unbreakable{type = 'minecraft:unbreakable'}}.

