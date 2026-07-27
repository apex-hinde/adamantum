-module(component_encode).

-export([
	 encode_component/2
	]).
-include("src/data_types/components/component_records.hrl").

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
