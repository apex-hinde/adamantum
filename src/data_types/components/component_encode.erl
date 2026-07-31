-module(component_encode).

-include("src/data_types/components/component_records.hrl").
-export([
	 encode_component/2
	]).
encode_component(TypeId, Data) when is_integer(TypeId) ->
    Name = component_type_registry:id_to_name(TypeId),
    encode(Name, Data);
encode_component(Name, Data) when is_atom(Name) ->
    encode(Name, Data).

encode(TypeId, Data) ->
    case TypeId of
	dye ->
	    encode_dye_color(Data);
	trim ->
	    encode_trim(Data);
	trim_pattern ->
	    encode_trim_pattern(Data);
	trim_material ->
	    encode_trim_material(Data);
	'minecraft:dye' ->
	    encode_dye_color(Data);
	'minecraft:trim' ->
	    encode_trim(Data);
	'minecraft:trim_pattern' ->
	    encode_trim_pattern(Data);
	'minecraft:trim_material' ->
	    encode_trim_material(Data);
	block_predicate ->
	    encode_block_predicate(Data);
	'minecraft:block_predicate' ->
	    encode_block_predicate(Data);
	block_predictate ->
	    encode_block_predicate(Data);
	property ->
	    encode_property(Data);
	exact_data_component_matcher ->
	    encode_exact_data_component_matcher(Data);
	partial_data_component_matcher ->
	    encode_partial_data_component_matcher(Data);
	firework_explosion ->
	    encode_firework_explosion(Data);
	'minecraft:firework_explosion' ->
	    encode_firework_explosion(Data);
	potion_effect ->
	    encode_potion_effect(Data);
	'minecraft:potion_effect' ->
	    encode_potion_effect(Data);
	potion_effect_detail ->
	    encode_potion_effect_detail(Data);
	'minecraft:potion_effect_detail' ->
	    encode_potion_effect_detail(Data);
	consume_effect ->
	    encode_consume_effect(Data);
	'minecraft:consume_effect' ->
	    encode_consume_effect(Data);
	'minecraft:can_place_on' ->
	    encode:encode_type(Data, {prefixed_array, block_predicate});
	'minecraft:can_break' ->
	    encode:encode_type(Data, {prefixed_array, block_predicate});
	can_place_on ->
	    encode:encode_type(Data, {prefixed_array, block_predicate});
	can_break ->
	    encode:encode_type(Data, {prefixed_array, block_predicate});
	'minecraft:wolf/collar' ->
	    encode_dye_color(Data);
	'minecraft:cat/collar' ->
	    encode_dye_color(Data);
	'minecraft:sheep/color' ->
	    encode_dye_color(Data);
	'minecraft:shulker/color' ->
	    encode_dye_color(Data);
	'minecraft:tropical_fish/base_color' ->
	    encode_dye_color(Data);
	'minecraft:tropical_fish/pattern_color' ->
	    encode_dye_color(Data);
	'minecraft:painting/variant' ->
	    encode_painting_variant(Data);
	painting_variant ->
	    encode_painting_variant(Data);
	instrument ->
	    encode_instrument(Data);
	'minecraft:instrument' ->
	    encode:encode_type(Data, {id_or_x, instrument});
	jukebox_song ->
	    encode_jukebox_song(Data);
	'minecraft:jukebox_song' ->
	    encode_jukebox_song(Data);
	jukebox_playable ->
	    encode:encode_type(Data, {id_or_x, jukebox_song});
	'minecraft:jukebox_playable' ->
	    encode:encode_type(Data, {id_or_x, jukebox_song});
	banner_pattern ->
	    encode_banner_pattern(Data);
	'minecraft:banner_pattern' ->
	    encode_banner_pattern(Data);
	'minecraft:provides_banner_patterns' ->
	    encode:encode_type(Data, id_set);
	provides_banner_patterns ->
	    encode:encode_type(Data, id_set);
	'minecraft:banner_patterns' ->
	    encode:encode_type(Data, {prefixed_array, [{id_or_x, banner_pattern}, dye]});
	banner_patterns ->
	    encode:encode_type(Data, {prefixed_array, [{id_or_x, banner_pattern}, dye]});
	custom_data ->
	    encode_custom_data(Data);
	'minecraft:custom_data' ->
	    encode_custom_data(Data);
	max_stack_size ->
	    encode_max_stack_size(Data);
	'minecraft:max_stack_size' ->
	    encode_max_stack_size(Data);
	max_damage ->
	    encode_max_damage(Data);
	'minecraft:max_damage' ->
	    encode_max_damage(Data);
	damage ->
	    encode_damage(Data);
	'minecraft:damage' ->
	    encode_damage(Data);
	unbreakable ->
	    encode_unbreakable(Data);
	'minecraft:unbreakable' ->
	    encode_unbreakable(Data);

	%% Text / identifier components
	custom_name -> encode_custom_name(Data);
	'minecraft:custom_name' -> encode_custom_name(Data);
	item_name -> encode_item_name(Data);
	'minecraft:item_name' -> encode_item_name(Data);
	item_model -> encode_item_model(Data);
	'minecraft:item_model' -> encode_item_model(Data);
	lore -> encode_lore(Data);
	'minecraft:lore' -> encode_lore(Data);
	rarity -> encode_rarity(Data);
	'minecraft:rarity' -> encode_rarity(Data);
	tooltip_style -> encode_tooltip_style(Data);
	'minecraft:tooltip_style' -> encode_tooltip_style(Data);
	note_block_sound -> encode_note_block_sound(Data);
	'minecraft:note_block_sound' -> encode_note_block_sound(Data);

	%% Simple integer / boolean components
	repair_cost -> encode_repair_cost(Data);
	'minecraft:repair_cost' -> encode_repair_cost(Data);
	creative_slot_lock -> encode_creative_slot_lock(Data);
	'minecraft:creative_slot_lock' -> encode_creative_slot_lock(Data);
	enchantment_glint_override -> encode_enchantment_glint_override(Data);
	'minecraft:enchantment_glint_override' -> encode_enchantment_glint_override(Data);
	glider -> encode_glider(Data);
	'minecraft:glider' -> encode_glider(Data);
	enchantable -> encode_enchantable(Data);
	'minecraft:enchantable' -> encode_enchantable(Data);
	map_id -> encode_map_id(Data);
	'minecraft:map_id' -> encode_map_id(Data);
	ominous_bottle_amplifier -> encode_ominous_bottle_amplifier(Data);
	'minecraft:ominous_bottle_amplifier' -> encode_ominous_bottle_amplifier(Data);
	potion_duration_scale -> encode_potion_duration_scale(Data);
	'minecraft:potion_duration_scale' -> encode_potion_duration_scale(Data);
	additional_trade_cost -> encode_additional_trade_cost(Data);
	'minecraft:additional_trade_cost' -> encode_additional_trade_cost(Data);
	minimum_attack_charge -> encode_minimum_attack_charge(Data);
	'minecraft:minimum_attack_charge' -> encode_minimum_attack_charge(Data);

	%% NBT-only components
	intangible_projectile -> encode_intangible_projectile(Data);
	'minecraft:intangible_projectile' -> encode_intangible_projectile(Data);
	map_decorations -> encode_map_decorations(Data);
	'minecraft:map_decorations' -> encode_map_decorations(Data);
	debug_stick_state -> encode_debug_stick_state(Data);
	'minecraft:debug_stick_state' -> encode_debug_stick_state(Data);
	bucket_entity_data -> encode_bucket_entity_data(Data);
	'minecraft:bucket_entity_data' -> encode_bucket_entity_data(Data);
	lock -> encode_lock(Data);
	'minecraft:lock' -> encode_lock(Data);
	container_loot -> encode_container_loot(Data);
	'minecraft:container_loot' -> encode_container_loot(Data);
	recipes -> encode_recipes(Data);
	'minecraft:recipes' -> encode_recipes(Data);

	%% Color / map components
	dyed_color -> encode_dyed_color(Data);
	'minecraft:dyed_color' -> encode_dyed_color(Data);
	map_color -> encode_map_color(Data);
	'minecraft:map_color' -> encode_map_color(Data);
	map_post_processing -> encode_map_post_processing(Data);
	'minecraft:map_post_processing' -> encode_map_post_processing(Data);
	base_color -> encode_base_color(Data);
	'minecraft:base_color' -> encode_base_color(Data);

	%% Slot-based components
	charged_projectiles -> encode_charged_projectiles(Data);
	'minecraft:charged_projectiles' -> encode_charged_projectiles(Data);
	bundle_contents -> encode_bundle_contents(Data);
	'minecraft:bundle_contents' -> encode_bundle_contents(Data);
	use_remainder -> encode_use_remainder(Data);
	'minecraft:use_remainder' -> encode_use_remainder(Data);
	container -> encode_container(Data);
	'minecraft:container' -> encode_container(Data);
	sulfur_cube_content -> encode_sulfur_cube_content(Data);
	'minecraft:sulfur_cube_content' -> encode_sulfur_cube_content(Data);

	%% Enchantment components
	enchantments -> encode_enchantments(Data);
	'minecraft:enchantments' -> encode_enchantments(Data);
	stored_enchantments -> encode_stored_enchantments(Data);
	'minecraft:stored_enchantments' -> encode_stored_enchantments(Data);

	%% Attribute modifiers
	attribute_modifiers -> encode_attribute_modifiers(Data);
	'minecraft:attribute_modifiers' -> encode_attribute_modifiers(Data);

	%% Custom model data
	custom_model_data -> encode_custom_model_data(Data);
	'minecraft:custom_model_data' -> encode_custom_model_data(Data);

	%% Tooltip display
	tooltip_display -> encode_tooltip_display(Data);
	'minecraft:tooltip_display' -> encode_tooltip_display(Data);

	%% Food and consumable
	food -> encode_food(Data);
	'minecraft:food' -> encode_food(Data);
	consumable -> encode_consumable(Data);
	'minecraft:consumable' -> encode_consumable(Data);

	%% Use cooldown / use effects
	use_cooldown -> encode_use_cooldown(Data);
	'minecraft:use_cooldown' -> encode_use_cooldown(Data);
	use_effects -> encode_use_effects(Data);
	'minecraft:use_effects' -> encode_use_effects(Data);

	%% Damage type / resistant
	damage_type -> encode_damage_type(Data);
	'minecraft:damage_type' -> encode_damage_type(Data);
	damage_resistant -> encode_damage_resistant(Data);
	'minecraft:damage_resistant' -> encode_damage_resistant(Data);

	%% Attack
	attack_range -> encode_attack_range(Data);
	'minecraft:attack_range' -> encode_attack_range(Data);
	weapon -> encode_weapon(Data);
	'minecraft:weapon' -> encode_weapon(Data);
	piercing_weapon -> encode_piercing_weapon(Data);
	'minecraft:piercing_weapon' -> encode_piercing_weapon(Data);
	kinetic_weapon -> encode_kinetic_weapon(Data);
	'minecraft:kinetic_weapon' -> encode_kinetic_weapon(Data);
	swing_animation -> encode_swing_animation(Data);
	'minecraft:swing_animation' -> encode_swing_animation(Data);

	%% Tool
	tool -> encode_tool(Data);
	'minecraft:tool' -> encode_tool(Data);

	%% Equippable / repairable
	equippable -> encode_equippable(Data);
	'minecraft:equippable' -> encode_equippable(Data);
	repairable -> encode_repairable(Data);
	'minecraft:repairable' -> encode_repairable(Data);

	%% Death protection / blocks attacks
	death_protection -> encode_death_protection(Data);
	'minecraft:death_protection' -> encode_death_protection(Data);
	blocks_attacks -> encode_blocks_attacks(Data);
	'minecraft:blocks_attacks' -> encode_blocks_attacks(Data);

	%% Potion contents
	potion_contents -> encode_potion_contents(Data);
	'minecraft:potion_contents' -> encode_potion_contents(Data);

	%% Suspicious stew
	suspicious_stew_effects -> encode_suspicious_stew_effects(Data);
	'minecraft:suspicious_stew_effects' -> encode_suspicious_stew_effects(Data);

	%% Book content
	writable_book_content -> encode_writable_book_content(Data);
	'minecraft:writable_book_content' -> encode_writable_book_content(Data);
	written_book_content -> encode_written_book_content(Data);
	'minecraft:written_book_content' -> encode_written_book_content(Data);

	%% Entity data
	entity_data -> encode_entity_data(Data);
	'minecraft:entity_data' -> encode_entity_data(Data);
	block_entity_data -> encode_block_entity_data(Data);
	'minecraft:block_entity_data' -> encode_block_entity_data(Data);

	%% Trim material provider
	provides_trim_material -> encode_provides_trim_material(Data);
	'minecraft:provides_trim_material' -> encode_provides_trim_material(Data);

	%% Fireworks
	fireworks -> encode_fireworks(Data);
	'minecraft:fireworks' -> encode_fireworks(Data);

	%% Profile
	profile_component -> encode_profile_component(Data);
	'minecraft:profile' -> encode_profile_component(Data);

	%% Lodestone tracker
	lodestone_tracker -> encode_lodestone_tracker(Data);
	'minecraft:lodestone_tracker' -> encode_lodestone_tracker(Data);

	%% Pot decorations / block state / bees
	pot_decorations -> encode_pot_decorations(Data);
	'minecraft:pot_decorations' -> encode_pot_decorations(Data);
	block_state -> encode_block_state(Data);
	'minecraft:block_state' -> encode_block_state(Data);
	bees -> encode_bees(Data);
	'minecraft:bees' -> encode_bees(Data);

	%% Break sound
	break_sound -> encode_break_sound(Data);
	'minecraft:break_sound' -> encode_break_sound(Data);

	%% Variant components
	'minecraft:villager/variant' -> encode_variant_component(Data);
	'minecraft:wolf/variant' -> encode_variant_component(Data);
	'minecraft:wolf/sound_variant' -> encode_variant_component(Data);
	'minecraft:fox/variant' -> encode_variant_component(Data);
	'minecraft:salmon/size' -> encode_variant_component(Data);
	'minecraft:parrot/variant' -> encode_variant_component(Data);
	'minecraft:tropical_fish/pattern' -> encode_variant_component(Data);
	'minecraft:mooshroom/variant' -> encode_variant_component(Data);
	'minecraft:rabbit/variant' -> encode_variant_component(Data);
	'minecraft:pig/variant' -> encode_variant_component(Data);
	'minecraft:pig/sound_variant' -> encode_variant_component(Data);
	'minecraft:cow/variant' -> encode_variant_component(Data);
	'minecraft:cow/sound_variant' -> encode_variant_component(Data);
	'minecraft:chicken/variant' -> encode_variant_component(Data);
	'minecraft:chicken/sound_variant' -> encode_variant_component(Data);
	'minecraft:zombie_nautilus/variant' -> encode_variant_component(Data);
	'minecraft:frog/variant' -> encode_variant_component(Data);
	'minecraft:horse/variant' -> encode_variant_component(Data);
	'minecraft:llama/variant' -> encode_variant_component(Data);
	'minecraft:axolotl/variant' -> encode_variant_component(Data);
	'minecraft:cat/variant' -> encode_variant_component(Data);
	'minecraft:cat/sound_variant' -> encode_variant_component(Data);

	_ ->
	    error({unimplemented_component_encoder, TypeId})
    end.






dye_colors() ->
    [white, orange, magenta, light_blue, yellow, lime, pink, gray,
     light_gray, cyan, purple, blue, brown, green, red, black].
encode_dye_color(Color) ->
    encode:encode_type(Color, {enum, varint, dye_colors()}).




encode_painting_variant(Id) when is_integer(Id) ->
    encode_painting_variant({id, Id});
encode_painting_variant({id, Id}) ->
    TagBin = encode:encode_type(0, varint),
    IdBin = encode:encode_type(Id, varint),
    <<TagBin/binary, IdBin/binary>>;
encode_painting_variant({inline, Map}) when is_map(Map) ->
    encode_painting_variant(Map);
encode_painting_variant(#{asset_id := AssetId, width := Width, height := Height, title := Title, author := Author}) ->
    TagBin = encode:encode_type(1, varint),
    AssetBin = encode:encode_type(AssetId, string),
    WidthBin = encode:encode_type(Width, varint),
    HeightBin = encode:encode_type(Height, varint),
    TitleBin = encode:encode_type(Title, string),
    AuthorBin = encode:encode_type(Author, string),
    <<TagBin/binary, AssetBin/binary, WidthBin/binary, HeightBin/binary, TitleBin/binary, AuthorBin/binary>>.

encode_trim(#trim{material = Material, pattern = Pattern}) ->
    MatBin = encode:encode_type(Material, {id_or_x, trim_material}),
    PatBin = encode:encode_type(Pattern, {id_or_x, trim_pattern}),
    <<MatBin/binary, PatBin/binary>>;
encode_trim(#{material := Material, pattern := Pattern}) ->
    MatBin = encode:encode_type(Material, {id_or_x, trim_material}),
    PatBin = encode:encode_type(Pattern, {id_or_x, trim_pattern}),
    <<MatBin/binary, PatBin/binary>>.

encode_trim_material(#trim_material{suffix = Suffix, overrides = Overrides, description = Description}) ->
    SuffixBin = encode:encode_type(Suffix, string),
    OverridesBin = encode:encode_type(Overrides, {prefixed_array, [identifier, string]}),
    DescBin = encode:encode_type(Description, text_component),
    <<SuffixBin/binary, OverridesBin/binary, DescBin/binary>>;
encode_trim_material(#{suffix := Suffix, overrides := Overrides, description := Description}) ->
    SuffixBin = encode:encode_type(Suffix, string),
    OverridesBin = encode:encode_type(Overrides, {prefixed_array, [identifier, string]}),
    DescBin = encode:encode_type(Description, text_component),
    <<SuffixBin/binary, OverridesBin/binary, DescBin/binary>>.

encode_trim_pattern(#trim_pattern{asset_name = AssetName, template_item = TemplateItem, description = Description, is_decal = Decal}) ->
    AssetBin = encode:encode_type(AssetName, string),
    ItemBin = encode:encode_type(TemplateItem, varint),
    DescBin = encode:encode_type(Description, text_component),
    DecalBin = encode:encode_type(Decal, bool),
    <<AssetBin/binary, ItemBin/binary, DescBin/binary, DecalBin/binary>>;
encode_trim_pattern(#{asset_name := AssetName, template_item := TemplateItem, description := Description, is_decal := Decal}) ->
    AssetBin = encode:encode_type(AssetName, string),
    ItemBin = encode:encode_type(TemplateItem, varint),
    DescBin = encode:encode_type(Description, text_component),
    DecalBin = encode:encode_type(Decal, bool),
    <<AssetBin/binary, ItemBin/binary, DescBin/binary, DecalBin/binary>>.

encode_block_predicate(#block_predicate{
	blocks = Blocks,
	properties = Properties,
	nbt = NBT,
	data_components = DataComponents,
	partial_data_components = PartialDataComponents
}) ->
	encode_block_predicate(Blocks, Properties, NBT, DataComponents, PartialDataComponents);
encode_block_predicate(#{
	blocks := Blocks,
	properties := Properties,
	nbt := NBT,
	data_components := DataComponents,
	partial_data_components := PartialDataComponents
}) ->
	encode_block_predicate(Blocks, Properties, NBT, DataComponents, PartialDataComponents);
encode_block_predicate(Map) when is_map(Map) ->
	Blocks = maps:get(blocks, Map, none),
	Properties = maps:get(properties, Map, none),
	NBT = maps:get(nbt, Map, none),
	DataComponents = maps:get(data_components, Map, []),
	PartialDataComponents = maps:get(partial_data_components, Map, []),
	encode_block_predicate(Blocks, Properties, NBT, DataComponents, PartialDataComponents).

encode_block_predicate(Blocks, Properties, NBT, DataComponents, PartialDataComponents) ->
	BlocksBin = encode:encode_type(Blocks, {prefixed_optional, id_set}),
	PropertiesBin = encode:encode_type(Properties, {prefixed_optional, {prefixed_array, property}}),
	NBTBin = encode:encode_type(NBT, {prefixed_optional, nbt}),
	DCBin = encode:encode_type(DataComponents, {prefixed_array, exact_data_component_matcher}),
	PDCBin = encode:encode_type(PartialDataComponents, {prefixed_array, partial_data_component_matcher}),
	<<BlocksBin/binary, PropertiesBin/binary, NBTBin/binary, DCBin/binary, PDCBin/binary>>.

encode_property(#{name := Name, is_exact_match := true, exact_value := ExactValue}) ->
	NameBin = encode:encode_type(Name, string),
	ExactBool = encode:encode_type(true, bool),
	ValBin = encode:encode_type(ExactValue, string),
	<<NameBin/binary, ExactBool/binary, ValBin/binary>>;
encode_property(#{name := Name, exact_value := ExactValue}) ->
	encode_property(#{name => Name, is_exact_match => true, exact_value => ExactValue});
encode_property(#{name := Name, is_exact_match := false, min_value := MinValue, max_value := MaxValue}) ->
	NameBin = encode:encode_type(Name, string),
	ExactBool = encode:encode_type(false, bool),
	MinBin = encode:encode_type(MinValue, string),
	MaxBin = encode:encode_type(MaxValue, string),
	<<NameBin/binary, ExactBool/binary, MinBin/binary, MaxBin/binary>>;
encode_property(#{name := Name, min_value := MinValue, max_value := MaxValue}) ->
	encode_property(#{name => Name, is_exact_match => false, min_value => MinValue, max_value => MaxValue});
encode_property({Name, ExactValue}) ->
	encode_property(#{name => Name, is_exact_match => true, exact_value => ExactValue});
encode_property({Name, MinValue, MaxValue}) ->
	encode_property(#{name => Name, is_exact_match => false, min_value => MinValue, max_value => MaxValue}).

encode_exact_data_component_matcher(#{type := Type, value := Value}) ->
	encode_exact_data_component_matcher({Type, Value});
encode_exact_data_component_matcher({Type, Value}) ->
	TypeId = case is_integer(Type) of
		true -> Type;
		false -> component_type_registry:name_to_id(Type)
	end,
	TypeBin = encode:encode_type(TypeId, varint),
	ValueBin = encode_component(TypeId, Value),
	<<TypeBin/binary, ValueBin/binary>>.

encode_partial_data_component_matcher(#{type := Type, predicate := Predicate}) ->
	encode_partial_data_component_matcher({Type, Predicate});
encode_partial_data_component_matcher({Type, Predicate}) ->
	TypeId = case is_integer(Type) of
		true -> Type;
		false ->
			try data_component_predicate_type_type_registry:name_to_id(Type)
			catch _:_ ->
				error({unknown_predicate_type, Type})
			end
	end,
	TypeBin = encode:encode_type(TypeId, varint),
	PredicateBin = encode:encode_type(Predicate, nbt),
	<<TypeBin/binary, PredicateBin/binary>>.

firework_explosion_shapes() ->
    [small_ball, large_ball, star, creeper, burst].

encode_firework_explosion(#firework_explosion{
    shape = Shape,
    colors = Colors,
    fade_colors = FadeColors,
    has_trail = HasTrail,
    has_twinkle = HasTwinkle
}) ->
    encode_firework_explosion(Shape, Colors, FadeColors, HasTrail, HasTwinkle);
encode_firework_explosion(#{
    shape := Shape,
    colors := Colors,
    fade_colors := FadeColors,
    has_trail := HasTrail,
    has_twinkle := HasTwinkle
}) ->
    encode_firework_explosion(Shape, Colors, FadeColors, HasTrail, HasTwinkle);
encode_firework_explosion(Map) when is_map(Map) ->
    Shape = maps:get(shape, Map, small_ball),
    Colors = maps:get(colors, Map, []),
    FadeColors = maps:get(fade_colors, Map, []),
    HasTrail = maps:get(has_trail, Map, false),
    HasTwinkle = maps:get(has_twinkle, Map, false),
    encode_firework_explosion(Shape, Colors, FadeColors, HasTrail, HasTwinkle).

encode_firework_explosion(Shape, Colors, FadeColors, HasTrail, HasTwinkle) ->
    ShapeBin = encode:encode_type(Shape, {enum, varint, firework_explosion_shapes()}),
    ColorsBin = encode:encode_type(Colors, {prefixed_array, int}),
    FadeColorsBin = encode:encode_type(FadeColors, {prefixed_array, int}),
    TrailBin = encode:encode_type(HasTrail, bool),
    TwinkleBin = encode:encode_type(HasTwinkle, bool),
    <<ShapeBin/binary, ColorsBin/binary, FadeColorsBin/binary, TrailBin/binary, TwinkleBin/binary>>.

encode_potion_effect(#potion_effect{id = Id, details = Details}) ->
    encode_potion_effect(Id, Details);
encode_potion_effect(#{id := Id, details := Details}) ->
    encode_potion_effect(Id, Details);
encode_potion_effect(#{id := Id, detail := Detail}) ->
    encode_potion_effect(Id, Detail);
encode_potion_effect(#{type_id := TypeId, details := Details}) ->
    encode_potion_effect(TypeId, Details);
encode_potion_effect(#{type_id := TypeId, detail := Detail}) ->
    encode_potion_effect(TypeId, Detail);
encode_potion_effect(Map) when is_map(Map) ->
    Id = case Map of
       #{id := Val1} ->
           Val1;
       #{} ->
           maps:get(type_id, Map, 0)
   end,
    Details = case Map of
       #{details := Val2} ->
           Val2;
       #{} ->
           maps:get(detail, Map, #{})
   end,
    encode_potion_effect(Id, Details).

encode_potion_effect(Id, Details) ->
    TypeIdInt = case is_integer(Id) of
        true -> Id;
        false ->
            try mob_effect_type_registry:name_to_id(Id)
            catch _:_ -> error({unknown_mob_effect, Id})
            end
    end,
    IdBin = encode:encode_type(TypeIdInt, varint),
    DetailBin = encode_potion_effect_detail(Details),
    <<IdBin/binary, DetailBin/binary>>.

encode_potion_effect_detail(#potion_effect_detail{
    amplifier = Amplifier,
    duration = Duration,
    ambient = Ambient,
    show_particles = ShowParticles,
    show_icon = ShowIcon,
    hidden_effect = HiddenEffect
}) ->
    encode_potion_effect_detail(Amplifier, Duration, Ambient, ShowParticles, ShowIcon, HiddenEffect);
encode_potion_effect_detail(#{
    amplifier := Amplifier,
    duration := Duration,
    ambient := Ambient,
    show_particles := ShowParticles,
    show_icon := ShowIcon,
    hidden_effect := HiddenEffect
}) ->
    encode_potion_effect_detail(Amplifier, Duration, Ambient, ShowParticles, ShowIcon, HiddenEffect);
encode_potion_effect_detail(Map) when is_map(Map) ->
    Amplifier = maps:get(amplifier, Map, 0),
    Duration = maps:get(duration, Map, 0),
    Ambient = maps:get(ambient, Map, false),
    ShowParticles = maps:get(show_particles, Map, true),
    ShowIcon = maps:get(show_icon, Map, true),
    HiddenEffect = maps:get(hidden_effect, Map, none),
    encode_potion_effect_detail(Amplifier, Duration, Ambient, ShowParticles, ShowIcon, HiddenEffect).

encode_potion_effect_detail(Amplifier, Duration, Ambient, ShowParticles, ShowIcon, HiddenEffect) ->
    AmpBin = encode:encode_type(Amplifier, varint),
    DurBin = encode:encode_type(Duration, varint),
    AmbBin = encode:encode_type(Ambient, bool),
    PartBin = encode:encode_type(ShowParticles, bool),
    IconBin = encode:encode_type(ShowIcon, bool),
    HiddenBin = encode:encode_type(HiddenEffect, {prefixed_optional, potion_effect_detail}),
    <<AmpBin/binary, DurBin/binary, AmbBin/binary, PartBin/binary, IconBin/binary, HiddenBin/binary>>.

encode_consume_effect(#consume_effect{
    effect_type = EffectType,
    effects = Effects,
    probability = Probability,
    diameter = Diameter,
    sound = Sound
}) ->
    encode_consume_effect_data(EffectType, #{
        effects => Effects,
        probability => Probability,
        diameter => Diameter,
        sound => Sound
    });
encode_consume_effect(#{effect_type := EffectType} = Map) ->
    encode_consume_effect_data(EffectType, Map);
encode_consume_effect(#{type := EffectType} = Map) ->
    encode_consume_effect_data(EffectType, Map);
encode_consume_effect({EffectType, Data}) ->
    encode_consume_effect_data(EffectType, Data);
encode_consume_effect(Map) when is_map(Map) ->
    EffectType = maps:get(effect_type, Map, maps:get(type, Map, 'minecraft:clear_all_effects')),
    encode_consume_effect_data(EffectType, Map).

encode_consume_effect_data(EffectType, Data) ->
    TypeIdInt = case is_integer(EffectType) of
        true -> EffectType;
        false ->
            try consume_effect_type_type_registry:name_to_id(EffectType)
            catch _:_ -> error({unknown_consume_effect_type, EffectType})
            end
    end,
    TypeBin = encode:encode_type(TypeIdInt, varint),
    EffectTypeAtom = try consume_effect_type_type_registry:id_to_name(TypeIdInt)
                     catch _:_ -> TypeIdInt
                     end,
    DataBin = case EffectTypeAtom of
        'minecraft:apply_effects' -> encode_apply_effects(Data);
        'minecraft:remove_effects' -> encode_remove_effects(Data);
        'minecraft:clear_all_effects' -> encode_clear_all_effects(Data);
        'minecraft:teleport_randomly' -> encode_teleport_randomly(Data);
        'minecraft:play_sound' -> encode_play_sound(Data);
        _ -> error({unknown_consume_effect_type, EffectType})
    end,
    <<TypeBin/binary, DataBin/binary>>.

encode_apply_effects(Data) ->
    Effects = get_field(effects, Data, []),
    Prob = get_field(probability, Data, 1.0),
    EffectsBin = encode:encode_type(Effects, {prefixed_array, potion_effect}),
    ProbBin = encode:encode_type(Prob, float),
    <<EffectsBin/binary, ProbBin/binary>>.

encode_remove_effects(Data) ->
    Effects = get_field(effects, Data, []),
    encode:encode_type(Effects, id_set).

encode_clear_all_effects(_Data) ->
    <<>>.

encode_teleport_randomly(Data) ->
    Diameter = get_field(diameter, Data, 16.0),
    encode:encode_type(Diameter, float).

encode_play_sound(Data) ->
    Sound = get_field(sound, Data, {"minecraft:entity.generic.consume", false}),
    encode:encode_type(Sound, sound_event).

get_field(Key, Map, Default) when is_map(Map) ->
    case maps:get(Key, Map, Default) of
        undefined -> Default;
        Val -> Val
    end;
get_field(_Key, _Other, Default) ->
    Default.

encode_instrument(#instrument{
    sound_event = SoundEvent,
    use_duration = UseDuration,
    range = Range,
    description = Description
}) ->
    encode_instrument(SoundEvent, UseDuration, Range, Description);
encode_instrument(#{
    sound_event := SoundEvent,
    use_duration := UseDuration,
    range := Range,
    description := Description
}) ->
    encode_instrument(SoundEvent, UseDuration, Range, Description);
encode_instrument(Map) when is_map(Map) ->
    SoundEvent = maps:get(sound_event, Map, {id, 0}),
    UseDuration = maps:get(use_duration, Map, 0.0),
    Range = maps:get(range, Map, 16.0),
    Description = maps:get(description, Map, <<"">>),
    encode_instrument(SoundEvent, UseDuration, Range, Description).

encode_instrument(SoundEvent, UseDuration, Range, Description) ->
    SoundBin = encode:encode_type(SoundEvent, {id_or_x, sound_event}),
    UseDurBin = encode:encode_type(UseDuration, float),
    RangeBin = encode:encode_type(Range, float),
    DescBin = encode:encode_type(Description, text_component),
    <<SoundBin/binary, UseDurBin/binary, RangeBin/binary, DescBin/binary>>.

encode_jukebox_song(#jukebox_song{
    sound_event = SoundEvent,
    description = Description,
    duration = Duration,
    output = Output
}) ->
    encode_jukebox_song(SoundEvent, Description, Duration, Output);
encode_jukebox_song(#{
    sound_event := SoundEvent,
    description := Description,
    duration := Duration,
    output := Output
}) ->
    encode_jukebox_song(SoundEvent, Description, Duration, Output);
encode_jukebox_song(Map) when is_map(Map) ->
    SoundEvent = maps:get(sound_event, Map, {id, 0}),
    Description = maps:get(description, Map, <<"">>),
    Duration = maps:get(duration, Map, 0.0),
    Output = maps:get(output, Map, 0),
    encode_jukebox_song(SoundEvent, Description, Duration, Output).

encode_jukebox_song(SoundEvent, Description, Duration, Output) ->
    SoundBin = encode:encode_type(SoundEvent, {id_or_x, sound_event}),
    DescBin = encode:encode_type(Description, text_component),
    DurBin = encode:encode_type(Duration, float),
    OutputBin = encode:encode_type(Output, varint),
    <<SoundBin/binary, DescBin/binary, DurBin/binary, OutputBin/binary>>.

encode_banner_pattern(#banner_pattern{
    asset_id = AssetId,
    translation_key = TranslationKey
}) ->
    encode_banner_pattern(AssetId, TranslationKey);
encode_banner_pattern(#{
    asset_id := AssetId,
    translation_key := TranslationKey
}) ->
    encode_banner_pattern(AssetId, TranslationKey);
encode_banner_pattern(Map) when is_map(Map) ->
    AssetId = maps:get(asset_id, Map, <<"">>),
    TranslationKey = maps:get(translation_key, Map, <<"">>),
    encode_banner_pattern(AssetId, TranslationKey).

encode_banner_pattern(AssetId, TranslationKey) ->
    AssetBin = encode:encode_type(AssetId, identifier),
    TransKeyBin = encode:encode_type(TranslationKey, string),
    <<AssetBin/binary, TransKeyBin/binary>>.


%% ---- New component encoders ----

rarity_values() -> [common, uncommon, rare, epic].
consumable_animations() -> [none, eat, drink, block, bow, spear, crossbow, spyglass, toot_horn, brush].
attribute_operations() -> [add, multiply_base, multiply_total].
attribute_slots() -> [any, main_hand, off_hand, hand, feet, legs, chest, head, armor, body].
map_post_processing_types() -> [lock, scale].
equippable_slots() -> [mainhand, feet, legs, chest, head, offhand, body].
swing_animation_types() -> [none, whack, stab].

encode_custom_name(#custom_name{name = Name}) -> encode:encode_type(Name, text_component);
encode_custom_name(#{name := Name}) -> encode:encode_type(Name, text_component);
encode_custom_name(Name) -> encode:encode_type(Name, text_component).

encode_item_name(#item_name{name = Name}) -> encode:encode_type(Name, text_component);
encode_item_name(#{name := Name}) -> encode:encode_type(Name, text_component);
encode_item_name(Name) -> encode:encode_type(Name, text_component).

encode_item_model(#item_model{model = Model}) -> encode:encode_type(Model, identifier);
encode_item_model(#{model := Model}) -> encode:encode_type(Model, identifier);
encode_item_model(Model) -> encode:encode_type(Model, identifier).

encode_lore(#lore{lines = Lines}) -> encode:encode_type(Lines, {prefixed_array, text_component});
encode_lore(#{lines := Lines}) -> encode:encode_type(Lines, {prefixed_array, text_component});
encode_lore(Lines) when is_list(Lines) -> encode:encode_type(Lines, {prefixed_array, text_component}).

encode_rarity(#rarity{rarity = Rarity}) -> encode:encode_type(Rarity, {enum, varint, rarity_values()});
encode_rarity(#{rarity := Rarity}) -> encode:encode_type(Rarity, {enum, varint, rarity_values()});
encode_rarity(Rarity) -> encode:encode_type(Rarity, {enum, varint, rarity_values()}).

encode_tooltip_style(#tooltip_style{style = Style}) -> encode:encode_type(Style, identifier);
encode_tooltip_style(#{style := Style}) -> encode:encode_type(Style, identifier);
encode_tooltip_style(Style) -> encode:encode_type(Style, identifier).

encode_note_block_sound(#note_block_sound{sound = Sound}) -> encode:encode_type(Sound, identifier);
encode_note_block_sound(#{sound := Sound}) -> encode:encode_type(Sound, identifier);
encode_note_block_sound(Sound) -> encode:encode_type(Sound, identifier).

encode_repair_cost(#repair_cost{cost = Cost}) -> encode:encode_type(Cost, varint);
encode_repair_cost(#{cost := Cost}) -> encode:encode_type(Cost, varint);
encode_repair_cost(Cost) when is_integer(Cost) -> encode:encode_type(Cost, varint).

encode_creative_slot_lock(#creative_slot_lock{}) -> <<>>;
encode_creative_slot_lock(_) -> <<>>.

encode_enchantment_glint_override(#enchantment_glint_override{has_glint = HasGlint}) -> encode:encode_type(HasGlint, bool);
encode_enchantment_glint_override(#{has_glint := HasGlint}) -> encode:encode_type(HasGlint, bool);
encode_enchantment_glint_override(HasGlint) when is_boolean(HasGlint) -> encode:encode_type(HasGlint, bool).

encode_glider(#glider{}) -> <<>>;
encode_glider(_) -> <<>>.

encode_enchantable(#enchantable{value = Value}) -> encode:encode_type(Value, varint);
encode_enchantable(#{value := Value}) -> encode:encode_type(Value, varint);
encode_enchantable(Value) when is_integer(Value) -> encode:encode_type(Value, varint).

encode_map_id(#map_id{id = Id}) -> encode:encode_type(Id, varint);
encode_map_id(#{id := Id}) -> encode:encode_type(Id, varint);
encode_map_id(Id) when is_integer(Id) -> encode:encode_type(Id, varint).

encode_ominous_bottle_amplifier(#ominous_bottle_amplifier{amplifier = Amp}) -> encode:encode_type(Amp, varint);
encode_ominous_bottle_amplifier(#{amplifier := Amp}) -> encode:encode_type(Amp, varint);
encode_ominous_bottle_amplifier(Amp) when is_integer(Amp) -> encode:encode_type(Amp, varint).

encode_potion_duration_scale(#potion_duration_scale{scale = Scale}) -> encode:encode_type(Scale, float);
encode_potion_duration_scale(#{scale := Scale}) -> encode:encode_type(Scale, float);
encode_potion_duration_scale(Scale) when is_number(Scale) -> encode:encode_type(Scale, float).

encode_additional_trade_cost(#additional_trade_cost{cost = Cost}) -> encode:encode_type(Cost, varint);
encode_additional_trade_cost(#{cost := Cost}) -> encode:encode_type(Cost, varint);
encode_additional_trade_cost(Cost) when is_integer(Cost) -> encode:encode_type(Cost, varint).

encode_minimum_attack_charge(#minimum_attack_charge{charge = Charge}) -> encode:encode_type(Charge, float);
encode_minimum_attack_charge(#{charge := Charge}) -> encode:encode_type(Charge, float);
encode_minimum_attack_charge(Charge) when is_number(Charge) -> encode:encode_type(Charge, float).

encode_intangible_projectile(#intangible_projectile{data = Data}) -> encode:encode_type(Data, nbt);
encode_intangible_projectile(#{data := Data}) -> encode:encode_type(Data, nbt);
encode_intangible_projectile(Data) -> encode:encode_type(Data, nbt).

encode_map_decorations(#map_decorations{data = Data}) -> encode:encode_type(Data, nbt);
encode_map_decorations(#{data := Data}) -> encode:encode_type(Data, nbt);
encode_map_decorations(Data) -> encode:encode_type(Data, nbt).

encode_debug_stick_state(#debug_stick_state{data = Data}) -> encode:encode_type(Data, nbt);
encode_debug_stick_state(#{data := Data}) -> encode:encode_type(Data, nbt);
encode_debug_stick_state(Data) -> encode:encode_type(Data, nbt).

encode_bucket_entity_data(#bucket_entity_data{data = Data}) -> encode:encode_type(Data, nbt);
encode_bucket_entity_data(#{data := Data}) -> encode:encode_type(Data, nbt);
encode_bucket_entity_data(Data) -> encode:encode_type(Data, nbt).

encode_lock(#lock{key = Key}) -> encode:encode_type(Key, nbt);
encode_lock(#{key := Key}) -> encode:encode_type(Key, nbt);
encode_lock(Key) -> encode:encode_type(Key, nbt).

encode_container_loot(#container_loot{data = Data}) -> encode:encode_type(Data, nbt);
encode_container_loot(#{data := Data}) -> encode:encode_type(Data, nbt);
encode_container_loot(Data) -> encode:encode_type(Data, nbt).

encode_recipes(#recipes{data = Data}) -> encode:encode_type(Data, nbt);
encode_recipes(#{data := Data}) -> encode:encode_type(Data, nbt);
encode_recipes(Data) -> encode:encode_type(Data, nbt).

encode_dyed_color(#dyed_color{color = Color}) -> encode:encode_type(Color, int);
encode_dyed_color(#{color := Color}) -> encode:encode_type(Color, int);
encode_dyed_color(Color) when is_integer(Color) -> encode:encode_type(Color, int).

encode_map_color(#map_color{color = Color}) -> encode:encode_type(Color, int);
encode_map_color(#{color := Color}) -> encode:encode_type(Color, int);
encode_map_color(Color) when is_integer(Color) -> encode:encode_type(Color, int).

encode_map_post_processing(#map_post_processing{processing_type = Type}) -> encode:encode_type(Type, {enum, varint, map_post_processing_types()});
encode_map_post_processing(#{processing_type := Type}) -> encode:encode_type(Type, {enum, varint, map_post_processing_types()});
encode_map_post_processing(Type) -> encode:encode_type(Type, {enum, varint, map_post_processing_types()}).

encode_base_color(#base_color{color = Color}) -> encode:encode_type(Color, {enum, varint, dye_colors()});
encode_base_color(#{color := Color}) -> encode:encode_type(Color, {enum, varint, dye_colors()});
encode_base_color(Color) -> encode:encode_type(Color, {enum, varint, dye_colors()}).

encode_charged_projectiles(#charged_projectiles{projectiles = Projectiles}) -> encode:encode_type(Projectiles, {prefixed_array, slot});
encode_charged_projectiles(#{projectiles := Projectiles}) -> encode:encode_type(Projectiles, {prefixed_array, slot});
encode_charged_projectiles(Projectiles) when is_list(Projectiles) -> encode:encode_type(Projectiles, {prefixed_array, slot}).

encode_bundle_contents(#bundle_contents{items = Items}) -> encode:encode_type(Items, {prefixed_array, slot});
encode_bundle_contents(#{items := Items}) -> encode:encode_type(Items, {prefixed_array, slot});
encode_bundle_contents(Items) when is_list(Items) -> encode:encode_type(Items, {prefixed_array, slot}).

encode_use_remainder(#use_remainder{remainder = Remainder}) -> encode:encode_type(Remainder, slot);
encode_use_remainder(#{remainder := Remainder}) -> encode:encode_type(Remainder, slot);
encode_use_remainder(Remainder) -> encode:encode_type(Remainder, slot).

encode_container(#container{items = Items}) -> encode:encode_type(Items, {prefixed_array, slot});
encode_container(#{items := Items}) -> encode:encode_type(Items, {prefixed_array, slot});
encode_container(Items) when is_list(Items) -> encode:encode_type(Items, {prefixed_array, slot}).

encode_sulfur_cube_content(#sulfur_cube_content{content = Content}) -> encode:encode_type(Content, slot);
encode_sulfur_cube_content(#{content := Content}) -> encode:encode_type(Content, slot);
encode_sulfur_cube_content(Content) -> encode:encode_type(Content, slot).

encode_enchantments(#enchantments{enchantments = Enchants}) -> encode:encode_type(Enchants, {prefixed_array, [varint, varint]});
encode_enchantments(#{enchantments := Enchants}) -> encode:encode_type(Enchants, {prefixed_array, [varint, varint]});
encode_enchantments(Enchants) when is_list(Enchants) -> encode:encode_type(Enchants, {prefixed_array, [varint, varint]}).

encode_stored_enchantments(#stored_enchantments{enchantments = Enchants}) -> encode:encode_type(Enchants, {prefixed_array, [varint, varint]});
encode_stored_enchantments(#{enchantments := Enchants}) -> encode:encode_type(Enchants, {prefixed_array, [varint, varint]});
encode_stored_enchantments(Enchants) when is_list(Enchants) -> encode:encode_type(Enchants, {prefixed_array, [varint, varint]}).

encode_attribute_modifiers(#attribute_modifiers{modifiers = Modifiers}) ->
    encode:encode_type(Modifiers, {prefixed_array, [varint, identifier, double, {enum, varint, attribute_operations()}, {enum, varint, attribute_slots()}]});
encode_attribute_modifiers(#{modifiers := Modifiers}) ->
    encode:encode_type(Modifiers, {prefixed_array, [varint, identifier, double, {enum, varint, attribute_operations()}, {enum, varint, attribute_slots()}]});
encode_attribute_modifiers(Modifiers) when is_list(Modifiers) ->
    encode:encode_type(Modifiers, {prefixed_array, [varint, identifier, double, {enum, varint, attribute_operations()}, {enum, varint, attribute_slots()}]}).

encode_custom_model_data(#custom_model_data{floats = Floats, flags = Flags, strings = Strings, colors = Colors}) ->
    encode_custom_model_data(Floats, Flags, Strings, Colors);
encode_custom_model_data(#{floats := Floats, flags := Flags, strings := Strings, colors := Colors}) ->
    encode_custom_model_data(Floats, Flags, Strings, Colors);
encode_custom_model_data(Map) when is_map(Map) ->
    encode_custom_model_data(maps:get(floats, Map, []), maps:get(flags, Map, []), maps:get(strings, Map, []), maps:get(colors, Map, [])).

encode_custom_model_data(Floats, Flags, Strings, Colors) ->
    FloatsBin = encode:encode_type(Floats, {prefixed_array, float}),
    FlagsBin = encode:encode_type(Flags, {prefixed_array, bool}),
    StringsBin = encode:encode_type(Strings, {prefixed_array, string}),
    ColorsBin = encode:encode_type(Colors, {prefixed_array, int}),
    <<FloatsBin/binary, FlagsBin/binary, StringsBin/binary, ColorsBin/binary>>.

encode_tooltip_display(#tooltip_display{hide_tooltip = HideTooltip, hidden_components = HiddenComponents}) ->
    encode_tooltip_display(HideTooltip, HiddenComponents);
encode_tooltip_display(#{hide_tooltip := HideTooltip, hidden_components := HiddenComponents}) ->
    encode_tooltip_display(HideTooltip, HiddenComponents);
encode_tooltip_display(Map) when is_map(Map) ->
    encode_tooltip_display(maps:get(hide_tooltip, Map, false), maps:get(hidden_components, Map, [])).

encode_tooltip_display(HideTooltip, HiddenComponents) ->
    HideBin = encode:encode_type(HideTooltip, bool),
    HiddenBin = encode:encode_type(HiddenComponents, {prefixed_array, varint}),
    <<HideBin/binary, HiddenBin/binary>>.

encode_food(#food{nutrition = Nutrition, saturation_modifier = Sat, can_always_eat = CanAlwaysEat}) ->
    encode_food(Nutrition, Sat, CanAlwaysEat);
encode_food(#{nutrition := Nutrition, saturation_modifier := Sat, can_always_eat := CanAlwaysEat}) ->
    encode_food(Nutrition, Sat, CanAlwaysEat);
encode_food(Map) when is_map(Map) ->
    encode_food(maps:get(nutrition, Map, 0), maps:get(saturation_modifier, Map, 0.0), maps:get(can_always_eat, Map, false)).

encode_food(Nutrition, Sat, CanAlwaysEat) ->
    NutBin = encode:encode_type(Nutrition, varint),
    SatBin = encode:encode_type(Sat, float),
    EatBin = encode:encode_type(CanAlwaysEat, bool),
    <<NutBin/binary, SatBin/binary, EatBin/binary>>.

encode_consumable(#consumable{consume_seconds = Secs, animation = Anim, sound = Sound, has_consume_particles = Particles, effects = Effects}) ->
    encode_consumable(Secs, Anim, Sound, Particles, Effects);
encode_consumable(#{consume_seconds := Secs, animation := Anim, sound := Sound, has_consume_particles := Particles, effects := Effects}) ->
    encode_consumable(Secs, Anim, Sound, Particles, Effects);
encode_consumable(Map) when is_map(Map) ->
    encode_consumable(maps:get(consume_seconds, Map, 1.6), maps:get(animation, Map, eat),
                      maps:get(sound, Map, {id, 0}), maps:get(has_consume_particles, Map, true),
                      maps:get(effects, Map, [])).

encode_consumable(Secs, Anim, Sound, Particles, Effects) ->
    SecsBin = encode:encode_type(Secs, float),
    AnimBin = encode:encode_type(Anim, {enum, varint, consumable_animations()}),
    SoundBin = encode:encode_type(Sound, {id_or_x, sound_event}),
    PartBin = encode:encode_type(Particles, bool),
    EffBin = encode:encode_type(Effects, {prefixed_array, consume_effect}),
    <<SecsBin/binary, AnimBin/binary, SoundBin/binary, PartBin/binary, EffBin/binary>>.

encode_use_cooldown(#use_cooldown{seconds = Secs, cooldown_group = Group}) ->
    encode_use_cooldown(Secs, Group);
encode_use_cooldown(#{seconds := Secs, cooldown_group := Group}) ->
    encode_use_cooldown(Secs, Group);
encode_use_cooldown(Map) when is_map(Map) ->
    encode_use_cooldown(maps:get(seconds, Map, 1.0), maps:get(cooldown_group, Map, none)).

encode_use_cooldown(Secs, Group) ->
    SecsBin = encode:encode_type(Secs, float),
    GroupBin = encode:encode_type(Group, {prefixed_optional, identifier}),
    <<SecsBin/binary, GroupBin/binary>>.

encode_use_effects(#use_effects{can_sprint = CanSprint, interact_vibrations = Vibrations, speed_multiplier = Speed}) ->
    encode_use_effects(CanSprint, Vibrations, Speed);
encode_use_effects(#{can_sprint := CanSprint, interact_vibrations := Vibrations, speed_multiplier := Speed}) ->
    encode_use_effects(CanSprint, Vibrations, Speed);
encode_use_effects(Map) when is_map(Map) ->
    encode_use_effects(maps:get(can_sprint, Map, true), maps:get(interact_vibrations, Map, true), maps:get(speed_multiplier, Map, 1.0)).

encode_use_effects(CanSprint, Vibrations, Speed) ->
    SprintBin = encode:encode_type(CanSprint, bool),
    VibBin = encode:encode_type(Vibrations, bool),
    SpeedBin = encode:encode_type(Speed, float),
    <<SprintBin/binary, VibBin/binary, SpeedBin/binary>>.

encode_damage_type(#damage_type{damage_type = DT}) -> encode:encode_type(DT, varint);
encode_damage_type(#{damage_type := DT}) -> encode:encode_type(DT, varint);
encode_damage_type(DT) when is_integer(DT) -> encode:encode_type(DT, varint).

encode_damage_resistant(#damage_resistant{types = Types}) -> encode:encode_type(Types, id_set);
encode_damage_resistant(#{types := Types}) -> encode:encode_type(Types, id_set);
encode_damage_resistant(Types) -> encode:encode_type(Types, id_set).

encode_attack_range(#attack_range{min_reach = MinReach, max_reach = MaxReach, min_creative_reach = MinCR, max_creative_reach = MaxCR, hitbox_margin = HM, mob_factor = MF}) ->
    encode_attack_range(MinReach, MaxReach, MinCR, MaxCR, HM, MF);
encode_attack_range(#{min_reach := MinReach, max_reach := MaxReach, min_creative_reach := MinCR, max_creative_reach := MaxCR, hitbox_margin := HM, mob_factor := MF}) ->
    encode_attack_range(MinReach, MaxReach, MinCR, MaxCR, HM, MF).

encode_attack_range(MinReach, MaxReach, MinCR, MaxCR, HM, MF) ->
    <<(encode:encode_type(MinReach, float))/binary, (encode:encode_type(MaxReach, float))/binary,
      (encode:encode_type(MinCR, float))/binary, (encode:encode_type(MaxCR, float))/binary,
      (encode:encode_type(HM, float))/binary, (encode:encode_type(MF, float))/binary>>.

encode_weapon(#weapon{damage_per_attack = DPA, disable_blocking_for = DBF}) ->
    <<(encode:encode_type(DPA, varint))/binary, (encode:encode_type(DBF, float))/binary>>;
encode_weapon(#{damage_per_attack := DPA, disable_blocking_for := DBF}) ->
    <<(encode:encode_type(DPA, varint))/binary, (encode:encode_type(DBF, float))/binary>>.

encode_piercing_weapon(#piercing_weapon{deals_knockback = DK, dismounts = D, sound = S, hit_sound = HS}) ->
    <<(encode:encode_type(DK, bool))/binary, (encode:encode_type(D, bool))/binary,
      (encode:encode_type(S, {prefixed_optional, sound_event}))/binary,
      (encode:encode_type(HS, {prefixed_optional, sound_event}))/binary>>;
encode_piercing_weapon(#{deals_knockback := DK, dismounts := D, sound := S, hit_sound := HS}) ->
    <<(encode:encode_type(DK, bool))/binary, (encode:encode_type(D, bool))/binary,
      (encode:encode_type(S, {prefixed_optional, sound_event}))/binary,
      (encode:encode_type(HS, {prefixed_optional, sound_event}))/binary>>.

encode_kinetic_weapon(#kinetic_weapon{contact_cooldown_ticks = CCT, delay_ticks = DT, dismount_conditions = DC,
                                      knockback_conditions = KC, damage_conditions = DamC,
                                      forward_movement = FM, damage_multiplier = DM, sound = S, hit_sound = HS}) ->
    encode_kinetic_weapon(CCT, DT, DC, KC, DamC, FM, DM, S, HS);
encode_kinetic_weapon(#{contact_cooldown_ticks := CCT, delay_ticks := DT, dismount_conditions := DC,
                        knockback_conditions := KC, damage_conditions := DamC,
                        forward_movement := FM, damage_multiplier := DM, sound := S, hit_sound := HS}) ->
    encode_kinetic_weapon(CCT, DT, DC, KC, DamC, FM, DM, S, HS).

encode_kinetic_weapon(CCT, DT, DC, KC, DamC, FM, DM, S, HS) ->
    <<(encode:encode_type(CCT, varint))/binary, (encode:encode_type(DT, varint))/binary,
      (encode:encode_type(DC, {prefixed_optional, nbt}))/binary,
      (encode:encode_type(KC, {prefixed_optional, nbt}))/binary,
      (encode:encode_type(DamC, {prefixed_optional, nbt}))/binary,
      (encode:encode_type(FM, float))/binary, (encode:encode_type(DM, float))/binary,
      (encode:encode_type(S, {prefixed_optional, sound_event}))/binary,
      (encode:encode_type(HS, {prefixed_optional, sound_event}))/binary>>.

encode_swing_animation(#swing_animation{animation_type = Type, duration = Duration}) ->
    <<(encode:encode_type(Type, {enum, varint, swing_animation_types()}))/binary, (encode:encode_type(Duration, varint))/binary>>;
encode_swing_animation(#{animation_type := Type, duration := Duration}) ->
    <<(encode:encode_type(Type, {enum, varint, swing_animation_types()}))/binary, (encode:encode_type(Duration, varint))/binary>>.

encode_tool(#tool{rules = Rules, default_mining_speed = DMS, damage_per_block = DPB, can_destroy_blocks_in_creative = CDBC}) ->
    encode_tool(Rules, DMS, DPB, CDBC);
encode_tool(#{rules := Rules, default_mining_speed := DMS, damage_per_block := DPB, can_destroy_blocks_in_creative := CDBC}) ->
    encode_tool(Rules, DMS, DPB, CDBC);
encode_tool(Map) when is_map(Map) ->
    encode_tool(maps:get(rules, Map, []), maps:get(default_mining_speed, Map, 1.0),
                maps:get(damage_per_block, Map, 0), maps:get(can_destroy_blocks_in_creative, Map, true)).

encode_tool(Rules, DMS, DPB, CDBC) ->
    RulesBin = encode:encode_type(Rules, {prefixed_array, [id_set, {prefixed_optional, float}, {prefixed_optional, bool}]}),
    DMSBin = encode:encode_type(DMS, float),
    DPBBin = encode:encode_type(DPB, varint),
    CDBCBin = encode:encode_type(CDBC, bool),
    <<RulesBin/binary, DMSBin/binary, DPBBin/binary, CDBCBin/binary>>.

encode_equippable(#equippable{slot = Slot, equip_sound = ES, model = Model, camera_overlay = CO,
                              allowed_entities = AE, dispensable = D, swappable = SW, damage_on_hurt = DOH,
                              can_be_sheared = CBS, shearing_sound = SS}) ->
    encode_equippable(Slot, ES, Model, CO, AE, D, SW, DOH, CBS, SS);
encode_equippable(#{slot := Slot, equip_sound := ES, model := Model, camera_overlay := CO,
                    allowed_entities := AE, dispensable := D, swappable := SW, damage_on_hurt := DOH,
                    can_be_sheared := CBS, shearing_sound := SS}) ->
    encode_equippable(Slot, ES, Model, CO, AE, D, SW, DOH, CBS, SS);
encode_equippable(Map) when is_map(Map) ->
    encode_equippable(maps:get(slot, Map, head), maps:get(equip_sound, Map, {id, 0}),
                      maps:get(model, Map, none), maps:get(camera_overlay, Map, none),
                      maps:get(allowed_entities, Map, none), maps:get(dispensable, Map, true),
                      maps:get(swappable, Map, true), maps:get(damage_on_hurt, Map, true),
                      maps:get(can_be_sheared, Map, false), maps:get(shearing_sound, Map, {id, 0})).

encode_equippable(Slot, ES, Model, CO, AE, D, SW, DOH, CBS, SS) ->
    <<(encode:encode_type(Slot, {enum, varint, equippable_slots()}))/binary,
      (encode:encode_type(ES, {id_or_x, sound_event}))/binary,
      (encode:encode_type(Model, {prefixed_optional, identifier}))/binary,
      (encode:encode_type(CO, {prefixed_optional, identifier}))/binary,
      (encode:encode_type(AE, {prefixed_optional, id_set}))/binary,
      (encode:encode_type(D, bool))/binary,
      (encode:encode_type(SW, bool))/binary,
      (encode:encode_type(DOH, bool))/binary,
      (encode:encode_type(CBS, bool))/binary,
      (encode:encode_type(SS, {id_or_x, sound_event}))/binary>>.

encode_repairable(#repairable{items = Items}) -> encode:encode_type(Items, id_set);
encode_repairable(#{items := Items}) -> encode:encode_type(Items, id_set);
encode_repairable(Items) -> encode:encode_type(Items, id_set).

encode_death_protection(#death_protection{effects = Effects}) -> encode:encode_type(Effects, {prefixed_array, consume_effect});
encode_death_protection(#{effects := Effects}) -> encode:encode_type(Effects, {prefixed_array, consume_effect});
encode_death_protection(Effects) when is_list(Effects) -> encode:encode_type(Effects, {prefixed_array, consume_effect}).

encode_blocks_attacks(#blocks_attacks{block_delay_seconds = BDS, disable_cooldown_scale = DCS,
                                      damage_reductions = DR, bypassed_by = BB,
                                      block_sound = BS, disable_sound = DS}) ->
    {Reductions, IDT, IDB, IDF} = expand_damage_reductions(DR),
    encode_blocks_attacks_data(BDS, DCS, Reductions, IDT, IDB, IDF, BB, BS, DS);
encode_blocks_attacks(#{block_delay_seconds := BDS, disable_cooldown_scale := DCS,
                         damage_reductions := DR, bypassed_by := BB,
                         block_sound := BS, disable_sound := DS}) ->
    {Reductions, IDT, IDB, IDF} = expand_damage_reductions(DR),
    encode_blocks_attacks_data(BDS, DCS, Reductions, IDT, IDB, IDF, BB, BS, DS).

expand_damage_reductions(DR) when is_map(DR) ->
    Reductions = maps:get(reductions, DR, []),
    IDT = maps:get(item_damage_threshold, DR, 0.0),
    IDB = maps:get(item_damage_base, DR, 0.0),
    IDF = maps:get(item_damage_factor, DR, 0.0),
    {Reductions, IDT, IDB, IDF};
expand_damage_reductions(DR) when is_list(DR) ->
    {DR, 0.0, 0.0, 0.0}.

encode_blocks_attacks_data(BDS, DCS, Reductions, IDT, IDB, IDF, BB, BS, DS) ->
    <<(encode:encode_type(BDS, float))/binary,
      (encode:encode_type(DCS, float))/binary,
      (encode:encode_type(Reductions, {prefixed_array, [float, {prefixed_optional, id_set}, float, float]}))/binary,
      (encode:encode_type(IDT, float))/binary,
      (encode:encode_type(IDB, float))/binary,
      (encode:encode_type(IDF, float))/binary,
      (encode:encode_type(BB, {prefixed_optional, id_set}))/binary,
      (encode:encode_type(BS, {prefixed_optional, {id_or_x, sound_event}}))/binary,
      (encode:encode_type(DS, {prefixed_optional, {id_or_x, sound_event}}))/binary>>.

encode_potion_contents(#potion_contents{potion_id = PotionId, custom_color = CC, custom_effects = CE, custom_name = CN}) ->
    encode_potion_contents(PotionId, CC, CE, CN);
encode_potion_contents(#{potion_id := PotionId, custom_color := CC, custom_effects := CE, custom_name := CN}) ->
    encode_potion_contents(PotionId, CC, CE, CN);
encode_potion_contents(Map) when is_map(Map) ->
    encode_potion_contents(maps:get(potion_id, Map, none), maps:get(custom_color, Map, none),
                           maps:get(custom_effects, Map, []), maps:get(custom_name, Map, none)).

encode_potion_contents(PotionId, CC, CE, CN) ->
    <<(encode:encode_type(PotionId, {prefixed_optional, varint}))/binary,
      (encode:encode_type(CC, {prefixed_optional, int}))/binary,
      (encode:encode_type(CE, {prefixed_array, potion_effect}))/binary,
      (encode:encode_type(CN, {prefixed_optional, string}))/binary>>.

encode_suspicious_stew_effects(#suspicious_stew_effects{effects = Effects}) ->
    encode:encode_type(Effects, {prefixed_array, [varint, varint]});
encode_suspicious_stew_effects(#{effects := Effects}) ->
    encode:encode_type(Effects, {prefixed_array, [varint, varint]});
encode_suspicious_stew_effects(Effects) when is_list(Effects) ->
    encode:encode_type(Effects, {prefixed_array, [varint, varint]}).

encode_writable_book_content(#writable_book_content{pages = Pages}) ->
    encode:encode_type(Pages, {prefixed_array, [string, {prefixed_optional, string}]});
encode_writable_book_content(#{pages := Pages}) ->
    encode:encode_type(Pages, {prefixed_array, [string, {prefixed_optional, string}]});
encode_writable_book_content(Pages) when is_list(Pages) ->
    encode:encode_type(Pages, {prefixed_array, [string, {prefixed_optional, string}]}).

encode_written_book_content(#written_book_content{raw_title = RT, filtered_title = FT, author = Author,
                                                   generation = Gen, pages = Pages, resolved = Resolved}) ->
    encode_written_book_content(RT, FT, Author, Gen, Pages, Resolved);
encode_written_book_content(#{raw_title := RT, filtered_title := FT, author := Author,
                               generation := Gen, pages := Pages, resolved := Resolved}) ->
    encode_written_book_content(RT, FT, Author, Gen, Pages, Resolved).

encode_written_book_content(RT, FT, Author, Gen, Pages, Resolved) ->
    <<(encode:encode_type(RT, string))/binary,
      (encode:encode_type(FT, {prefixed_optional, string}))/binary,
      (encode:encode_type(Author, string))/binary,
      (encode:encode_type(Gen, varint))/binary,
      (encode:encode_type(Pages, {prefixed_array, [text_component, {prefixed_optional, text_component}]}))/binary,
      (encode:encode_type(Resolved, bool))/binary>>.

encode_entity_data(#entity_data{entity_type = ET, data = Data}) ->
    <<(encode:encode_type(ET, varint))/binary, (encode:encode_type(Data, nbt))/binary>>;
encode_entity_data(#{entity_type := ET, data := Data}) ->
    <<(encode:encode_type(ET, varint))/binary, (encode:encode_type(Data, nbt))/binary>>.

encode_block_entity_data(#block_entity_data{block_entity_type = BET, data = Data}) ->
    <<(encode:encode_type(BET, varint))/binary, (encode:encode_type(Data, nbt))/binary>>;
encode_block_entity_data(#{block_entity_type := BET, data := Data}) ->
    <<(encode:encode_type(BET, varint))/binary, (encode:encode_type(Data, nbt))/binary>>.

encode_provides_trim_material(#provides_trim_material{key = Key}) -> encode:encode_type(Key, id_set);
encode_provides_trim_material(#{key := Key}) -> encode:encode_type(Key, id_set);
encode_provides_trim_material(Key) -> encode:encode_type(Key, id_set).

encode_fireworks(#fireworks{flight_duration = FD, explosions = Expl}) ->
    <<(encode:encode_type(FD, varint))/binary, (encode:encode_type(Expl, {prefixed_array, firework_explosion}))/binary>>;
encode_fireworks(#{flight_duration := FD, explosions := Expl}) ->
    <<(encode:encode_type(FD, varint))/binary, (encode:encode_type(Expl, {prefixed_array, firework_explosion}))/binary>>.

encode_profile_component(#profile_component{profile = Profile}) ->
    encode:encode_type(Profile, {resolvable_profile, false, false, false, false});
encode_profile_component(#{profile := Profile}) ->
    encode:encode_type(Profile, {resolvable_profile, false, false, false, false});
encode_profile_component(Profile) ->
    encode:encode_type(Profile, {resolvable_profile, false, false, false, false}).

encode_lodestone_tracker(#lodestone_tracker{has_global_position = true, dimension = Dim, position = Pos, tracked = Tracked}) ->
    <<(encode:encode_type(true, bool))/binary,
      (encode:encode_type(Dim, identifier))/binary,
      (encode:encode_type(Pos, position))/binary,
      (encode:encode_type(Tracked, bool))/binary>>;
encode_lodestone_tracker(#lodestone_tracker{has_global_position = false, tracked = Tracked}) ->
    <<(encode:encode_type(false, bool))/binary, (encode:encode_type(Tracked, bool))/binary>>;
encode_lodestone_tracker(#{has_global_position := true, dimension := Dim, position := Pos, tracked := Tracked}) ->
    <<(encode:encode_type(true, bool))/binary,
      (encode:encode_type(Dim, identifier))/binary,
      (encode:encode_type(Pos, position))/binary,
      (encode:encode_type(Tracked, bool))/binary>>;
encode_lodestone_tracker(#{has_global_position := false, tracked := Tracked}) ->
    <<(encode:encode_type(false, bool))/binary, (encode:encode_type(Tracked, bool))/binary>>.

encode_pot_decorations(#pot_decorations{decorations = Decorations}) ->
    encode:encode_type(Decorations, {prefixed_array, varint});
encode_pot_decorations(#{decorations := Decorations}) ->
    encode:encode_type(Decorations, {prefixed_array, varint});
encode_pot_decorations(Decorations) when is_list(Decorations) ->
    encode:encode_type(Decorations, {prefixed_array, varint}).

encode_block_state(#block_state{properties = Properties}) ->
    encode:encode_type(Properties, {prefixed_array, [string, string]});
encode_block_state(#{properties := Properties}) ->
    encode:encode_type(Properties, {prefixed_array, [string, string]});
encode_block_state(Properties) when is_list(Properties) ->
    encode:encode_type(Properties, {prefixed_array, [string, string]}).

encode_bees(#bees{bees = BeesData}) ->
    encode:encode_type(BeesData, {prefixed_array, [varint, nbt, varint, varint]});
encode_bees(#{bees := BeesData}) ->
    encode:encode_type(BeesData, {prefixed_array, [varint, nbt, varint, varint]});
encode_bees(BeesData) when is_list(BeesData) ->
    encode:encode_type(BeesData, {prefixed_array, [varint, nbt, varint, varint]}).

encode_break_sound(#break_sound{sound_event = SE}) ->
    encode:encode_type(SE, {id_or_x, sound_event});
encode_break_sound(#{sound_event := SE}) ->
    encode:encode_type(SE, {id_or_x, sound_event});
encode_break_sound(SE) ->
    encode:encode_type(SE, {id_or_x, sound_event}).

encode_variant_component(#variant_component{variant = Variant}) ->
    encode:encode_type(Variant, varint);
encode_variant_component(#{variant := Variant}) ->
    encode:encode_type(Variant, varint);
encode_variant_component(Variant) when is_integer(Variant) ->
    encode:encode_type(Variant, varint).



encode_custom_data(#custom_data{data = Data}) ->
    encode_custom_data(Data);
encode_custom_data(#{data := Data}) ->
    encode_custom_data(Data);
encode_custom_data(Map) when is_map(Map) ->
    Data = maps:get(data, Map, []),
    encode_custom_data(Data);
encode_custom_data(Data) ->
    encode:encode_type(Data, nbt).

encode_max_stack_size(#max_stack_size{max_stack_size = MaxStackSize}) ->
    encode_max_stack_size(MaxStackSize);
encode_max_stack_size(#{max_stack_size := MaxStackSize}) ->
    encode_max_stack_size(MaxStackSize);
encode_max_stack_size(Map) when is_map(Map) ->
    MaxStackSize = maps:get(max_stack_size, Map, 64),
    encode_max_stack_size(MaxStackSize);
encode_max_stack_size(MaxStackSize) when is_integer(MaxStackSize) ->
    encode:encode_type(MaxStackSize, varint).

encode_max_damage(#max_damage{max_damage = MaxDamage}) ->
    encode_max_damage(MaxDamage);
encode_max_damage(#{max_damage := MaxDamage}) ->
    encode_max_damage(MaxDamage);
encode_max_damage(Map) when is_map(Map) ->
    MaxDamage = maps:get(max_damage, Map, 0),
    encode_max_damage(MaxDamage);
encode_max_damage(MaxDamage) when is_integer(MaxDamage) ->
    encode:encode_type(MaxDamage, varint).

encode_damage(#damage{damage = Damage}) ->
    encode_damage(Damage);
encode_damage(#{damage := Damage}) ->
    encode_damage(Damage);
encode_damage(Map) when is_map(Map) ->
    Damage = maps:get(damage, Map, 0),
    encode_damage(Damage);
encode_damage(Damage) when is_integer(Damage) ->
    encode:encode_type(Damage, varint).

encode_unbreakable(#unbreakable{}) ->
    <<>>;
encode_unbreakable(Map) when is_map(Map) ->
    <<>>;
encode_unbreakable(_Data) ->
    <<>>.
