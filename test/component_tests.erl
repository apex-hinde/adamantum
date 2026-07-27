-module(component_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/data_types/components/component_records.hrl").
-include("src/data_types/records.hrl").

dye_color_sub_type_test() ->
    ?assertEqual(<<0>>, component_encode:encode_component('minecraft:dye', white)),
    ?assertEqual(<<14>>, component_encode:encode_component('minecraft:dye', red)),
    ?assertEqual(<<15>>, component_encode:encode_component('minecraft:dye', black)),
    ?assertEqual(<<14>>, component_encode:encode_component('minecraft:dye', 14)),
    ?assertEqual({<<>>, #dye{type = 'minecraft:dye', colour = white}}, component_decode:decode_component('minecraft:dye', <<0>>)),
    ?assertEqual({<<>>, #dye{type = 'minecraft:dye', colour = red}}, component_decode:decode_component('minecraft:dye', <<14>>)),
    ?assertEqual({<<>>, #dye{type = 'minecraft:dye', colour = black}}, component_decode:decode_component('minecraft:dye', <<15>>)),
    ?assertEqual({error, "invalid enum value"}, component_decode:decode_component('minecraft:dye', <<16>>)).

dye_color_component_dispatch_test() ->
    %% minecraft:dye ID is 43
    DyeId = component_type_registry:name_to_id('minecraft:dye'),
    ?assertEqual(43, DyeId),
    ?assertEqual(<<14>>, component_encode:encode_component(DyeId, red)),
    ?assertEqual(<<14>>, component_encode:encode_component('minecraft:dye', red)),
    ?assertEqual({<<>>, #dye{type = 'minecraft:dye', colour = red}}, component_decode:decode_component(DyeId, <<14>>)),
    ?assertEqual({<<>>, #dye{type = 'minecraft:dye', colour = red}}, component_decode:decode_component('minecraft:dye', <<14>>)),

    %% minecraft:sheep/color ID is 109
    SheepColorId = component_type_registry:name_to_id('minecraft:sheep/color'),
    ?assertEqual(109, SheepColorId),
    ?assertEqual(<<0>>, component_encode:encode_component(SheepColorId, white)),
    ?assertEqual({<<>>, #dye{type = 'minecraft:dye', colour = white}}, component_decode:decode_component(SheepColorId, <<0>>)).

painting_variant_sub_type_test() ->
    %% ID variant (Tag 0)
    EncodedId = component_encode:encode_component('minecraft:painting/variant', 5),
    ?assertEqual(<<0, 5>>, EncodedId),
    ?assertEqual({<<>>, #id_or_x{id_or_x = 5}}, component_decode:decode_component('minecraft:painting/variant', EncodedId)),

    %% Inline variant (Tag 1)
    VariantMap = #{
        asset_id => "minecraft:kebab",
        width => 1,
        height => 1,
        title => "\"Kebab\"",
        author => "\"Kristoffer Zetterstrand\""
    },
    EncodedInline = component_encode:encode_component('minecraft:painting/variant', VariantMap),
    ?assertEqual({<<>>, #id_or_x{id_or_x = VariantMap}}, component_decode:decode_component('minecraft:painting/variant', EncodedInline)).

painting_variant_dispatch_test() ->
    PaintingVariantId = component_type_registry:name_to_id('minecraft:painting/variant'),
    ?assertEqual(103, PaintingVariantId),
    Encoded = component_encode:encode_component(PaintingVariantId, 3),
    ?assertEqual(<<0, 3>>, Encoded),
    ?assertEqual({<<>>, #id_or_x{id_or_x = 3}}, component_decode:decode_component(PaintingVariantId, Encoded)),
    ?assertEqual({<<>>, #id_or_x{id_or_x = 3}}, component_decode:decode_component('minecraft:painting/variant', Encoded)).

trim_with_registry_ids_test() ->
    TrimRecord = #trim{
        type = 'minecraft:trim',
        material = {id, 0},
        pattern = {id, 4}
    },
    Encoded = component_encode:encode_component('minecraft:trim', TrimRecord),
    ?assertEqual(<<1, 5>>, Encoded),
    ExpectedTrimRecord = #trim{
        type = 'minecraft:trim',
        material = #id_or_x{id_or_x = 0},
        pattern = #id_or_x{id_or_x = 4}
    },
    ?assertEqual({<<>>, ExpectedTrimRecord}, component_decode:decode_component('minecraft:trim', Encoded)),

    EncType = encode:encode_type(TrimRecord, 'minecraft:trim'),
    ?assertEqual({<<>>, ExpectedTrimRecord}, decode:decode_type(EncType, 'minecraft:trim')).

trim_with_inline_definitions_test() ->
    MaterialInput = #trim_material{
        type = 'minecraft:trim_material',
        suffix = "amethyst",
        overrides = [{"minecraft:chainmail", "amethyst_chainmail"}],
        description = #{type => <<"text">>, text => <<"Amethyst">>}
    },
    ExpectedMaterial = #trim_material{
        type = 'minecraft:trim_material',
        suffix = "amethyst",
        overrides = [{#identifier{identifier = "minecraft:chainmail"}, #string{string = "amethyst_chainmail"}}],
        description = #text_component{component_map = #{type => <<"text">>, text => <<"Amethyst">>}}
    },
    PatternInput = #trim_pattern{
        type = 'minecraft:trim_pattern',
        asset_name = "sentry",
        template_item = 1459,
        description = #{type => <<"text">>, text => <<"Sentry">>},
        is_decal = false
    },
    ExpectedPattern = #trim_pattern{
        type = 'minecraft:trim_pattern',
        asset_name = "sentry",
        template_item = 1459,
        description = #text_component{component_map = #{type => <<"text">>, text => <<"Sentry">>}},
        is_decal = false
    },
    TrimRecord = #trim{
        type = 'minecraft:trim',
        material = MaterialInput,
        pattern = PatternInput
    },

    EncodedMaterial = component_encode:encode_component('minecraft:trim_material', MaterialInput),
    ?assertEqual({<<>>, ExpectedMaterial}, component_decode:decode_component('minecraft:trim_material', EncodedMaterial)),

    EncodedPattern = component_encode:encode_component('minecraft:trim_pattern', PatternInput),
    ?assertEqual({<<>>, ExpectedPattern}, component_decode:decode_component('minecraft:trim_pattern', EncodedPattern)),

    EncodedTrim = component_encode:encode_component('minecraft:trim', TrimRecord),
    ExpectedBin = <<0, EncodedMaterial/binary, 0, EncodedPattern/binary>>,
    ?assertEqual(ExpectedBin, EncodedTrim),

    ExpectedDecoded = #trim{
        type = 'minecraft:trim',
        material = #id_or_x{id_or_x = ExpectedMaterial},
        pattern = #id_or_x{id_or_x = ExpectedPattern}
    },
    ?assertEqual({<<>>, ExpectedDecoded}, component_decode:decode_component('minecraft:trim', EncodedTrim)),

    EncGeneric = encode:encode_type(TrimRecord, 'minecraft:trim'),
    ?assertEqual({<<>>, ExpectedDecoded}, decode:decode_type(EncGeneric, 'minecraft:trim')).

block_predicate_test() ->
    %% Empty block_predicate:
    EmptyBin = <<0, 0, 0, 0, 0>>,
    ExpectedEmpty = #block_predicate{
        type = 'minecraft:block_predicate',
        blocks = #prefixed_optional{some = none, prefixed_optional = none},
        properties = #prefixed_optional{some = none, prefixed_optional = none},
        nbt = #prefixed_optional{some = none, prefixed_optional = none},
        data_components = [],
        partial_data_components = []
    },
    ?assertEqual({<<>>, ExpectedEmpty}, component_decode:decode_component('minecraft:block_predicate', EmptyBin)),
    ?assertEqual({<<>>, ExpectedEmpty}, component_decode:decode_component(block_predicate, EmptyBin)),
    ?assertEqual({<<>>, ExpectedEmpty}, decode:decode_type(EmptyBin, block_predicate)),
    ?assertEqual({<<>>, ExpectedEmpty}, decode:decode_type(EmptyBin, 'minecraft:block_predicate')),

    %% Block predicate with properties:
    PropBin = <<0, 1, 1, 6, "facing", 1, 5, "north", 0, 0, 0>>,
    ExpectedProp = #block_predicate{
        type = 'minecraft:block_predicate',
        blocks = #prefixed_optional{some = none, prefixed_optional = none},
        properties = #prefixed_optional{some = some, prefixed_optional = #prefixed_array{prefixed_array = [#{name => "facing", is_exact_match => true, exact_value => "north"}]}},
        nbt = #prefixed_optional{some = none, prefixed_optional = none},
        data_components = [],
        partial_data_components = []
    },
    ?assertEqual({<<>>, ExpectedProp}, component_decode:decode_component('minecraft:block_predicate', PropBin)),

    %% can_place_on component (prefixed array of block_predicate):
    CanPlaceOnBin = <<1, 0, 0, 0, 0, 0>>,
    ?assertEqual({<<>>, #prefixed_array{prefixed_array = [ExpectedEmpty]}}, decode:decode_type(CanPlaceOnBin, 'minecraft:can_place_on')),

    %% can_break component (prefixed array of block_predicate):
    CanBreakBin = <<1, 0, 0, 0, 0, 0>>,
    ?assertEqual({<<>>, #prefixed_array{prefixed_array = [ExpectedEmpty]}}, decode:decode_type(CanBreakBin, 'minecraft:can_break')),

    %% Roundtrip encoding tests:
    EncodedEmpty = component_encode:encode_component('minecraft:block_predicate', ExpectedEmpty),
    ?assertEqual(EmptyBin, EncodedEmpty),

    EncodedProp = component_encode:encode_component('minecraft:block_predicate', ExpectedProp),
    ?assertEqual(PropBin, EncodedProp).

exact_data_component_matcher_test() ->
    %% TypeId=3 (damage), damage=42
    Data = <<3, 42>>,
    {Rest, Matcher} = component_decode:decode_component(exact_data_component_matcher, Data),
    ?assertEqual(<<>>, Rest),
    ?assertEqual(3, maps:get(type, Matcher)),
    ?assertEqual(#damage{type = 'minecraft:damage', damage = 42}, maps:get(value, Matcher)).

partial_data_component_matcher_test() ->
    NbtBin = nbt:encode([{tag_compound, [], [{tag_string, "key", "val"}]}]),
    Data = <<1, NbtBin/binary>>,
    {Rest, Matcher} = component_decode:decode_component(partial_data_component_matcher, Data),
    ?assertEqual(<<>>, Rest),
    ?assertEqual('minecraft:enchantments', maps:get(type, Matcher)),
    ExpectedNbt = nbt:decode(NbtBin),
    ?assertEqual(#nbt{nbt = ExpectedNbt}, maps:get(predicate, Matcher)).

firework_explosion_shapes_test() ->
    Shapes = [
        {small_ball, 0},
        {large_ball, 1},
        {star, 2},
        {creeper, 3},
        {burst, 4}
    ],
    lists:foreach(fun({ShapeAtom, ShapeInt}) ->
        Rec = #firework_explosion{
            type = 'minecraft:firework_explosion',
            shape = ShapeAtom,
            colors = [],
            fade_colors = [],
            has_trail = false,
            has_twinkle = false
        },
        Bin1 = component_encode:encode_component('minecraft:firework_explosion', Rec),
        Bin2 = component_encode:encode_component(firework_explosion, Rec),
        ?assertEqual(<<ShapeInt, 0, 0, 0, 0>>, Bin1),
        ?assertEqual(<<ShapeInt, 0, 0, 0, 0>>, Bin2),
        ?assertEqual({<<>>, Rec}, component_decode:decode_component('minecraft:firework_explosion', Bin1))
    end, Shapes).

potion_effect_detail_test() ->
    DetailWithoutHiddenInput = #potion_effect_detail{
        type = 'minecraft:potion_effect_detail',
        amplifier = 1,
        duration = 600,
        ambient = false,
        show_particles = true,
        show_icon = true,
        hidden_effect = none
    },
    DetailWithoutHiddenDecoded = #potion_effect_detail{
        type = 'minecraft:potion_effect_detail',
        amplifier = 1,
        duration = 600,
        ambient = false,
        show_particles = true,
        show_icon = true,
        hidden_effect = #prefixed_optional{some = none, prefixed_optional = none}
    },
    ExpectedBinWithoutHidden = <<1, 216, 4, 0, 1, 1, 0>>,

    EncodedDetail1 = component_encode:encode_component('minecraft:potion_effect_detail', DetailWithoutHiddenInput),
    ?assertEqual(ExpectedBinWithoutHidden, EncodedDetail1),
    ?assertEqual({<<>>, DetailWithoutHiddenDecoded}, component_decode:decode_component('minecraft:potion_effect_detail', ExpectedBinWithoutHidden)),
    ?assertEqual({<<>>, DetailWithoutHiddenDecoded}, decode:decode_type(ExpectedBinWithoutHidden, potion_effect_detail)),

    %% Nested hidden effect detail test
    HiddenDetailRecInput = #potion_effect_detail{
        type = 'minecraft:potion_effect_detail',
        amplifier = 0,
        duration = 1200,
        ambient = false,
        show_particles = true,
        show_icon = true,
        hidden_effect = none
    },
    HiddenDetailRecDecoded = #potion_effect_detail{
        type = 'minecraft:potion_effect_detail',
        amplifier = 0,
        duration = 1200,
        ambient = false,
        show_particles = true,
        show_icon = true,
        hidden_effect = #prefixed_optional{some = none, prefixed_optional = none}
    },
    DetailWithHiddenInput = #potion_effect_detail{
        type = 'minecraft:potion_effect_detail',
        amplifier = 2,
        duration = 300,
        ambient = true,
        show_particles = true,
        show_icon = false,
        hidden_effect = {some, HiddenDetailRecInput}
    },
    DetailWithHiddenDecoded = #potion_effect_detail{
        type = 'minecraft:potion_effect_detail',
        amplifier = 2,
        duration = 300,
        ambient = true,
        show_particles = true,
        show_icon = false,
        hidden_effect = #prefixed_optional{some = some, prefixed_optional = HiddenDetailRecDecoded}
    },
    EncodedHiddenDetail = component_encode:encode_component('minecraft:potion_effect_detail', HiddenDetailRecInput),
    ExpectedBinWithHidden = <<2, 172, 2, 1, 1, 0, 1, EncodedHiddenDetail/binary>>,

    EncodedDetail2 = component_encode:encode_component('minecraft:potion_effect_detail', DetailWithHiddenInput),
    ?assertEqual(ExpectedBinWithHidden, EncodedDetail2),
    ?assertEqual({<<>>, DetailWithHiddenDecoded}, component_decode:decode_component('minecraft:potion_effect_detail', ExpectedBinWithHidden)),
    ?assertEqual({<<>>, DetailWithHiddenDecoded}, decode:decode_type(ExpectedBinWithHidden, potion_effect_detail)).

potion_effect_test() ->
    DetailInput = #potion_effect_detail{
        type = 'minecraft:potion_effect_detail',
        amplifier = 0,
        duration = 600,
        ambient = false,
        show_particles = true,
        show_icon = true,
        hidden_effect = none
    },
    DetailDecoded = #potion_effect_detail{
        type = 'minecraft:potion_effect_detail',
        amplifier = 0,
        duration = 600,
        ambient = false,
        show_particles = true,
        show_icon = true,
        hidden_effect = #prefixed_optional{some = none, prefixed_optional = none}
    },
    EffectInput = #potion_effect{
        type = 'minecraft:potion_effect',
        id = 'minecraft:speed',
        details = DetailInput
    },
    EffectDecoded = #potion_effect{
        type = 'minecraft:potion_effect',
        id = 'minecraft:speed',
        details = DetailDecoded
    },

    ExpectedBin = <<0, 0, 216, 4, 0, 1, 1, 0>>,

    EncodedEffect = component_encode:encode_component('minecraft:potion_effect', EffectInput),
    ?assertEqual(ExpectedBin, EncodedEffect),
    ?assertEqual({<<>>, EffectDecoded}, component_decode:decode_component('minecraft:potion_effect', ExpectedBin)),

    %% Test map encoding
    MapEffect = #{
        id => 'minecraft:speed',
        details => DetailInput
    },
    ?assertEqual(ExpectedBin, component_encode:encode_component('minecraft:potion_effect', MapEffect)),

    %% Test dispatch via component_encode and component_decode
    ?assertEqual(ExpectedBin, component_encode:encode_component('minecraft:potion_effect', EffectInput)),
    ?assertEqual(ExpectedBin, component_encode:encode_component(potion_effect, EffectInput)),
    ?assertEqual({<<>>, EffectDecoded}, component_decode:decode_component('minecraft:potion_effect', ExpectedBin)).

consume_effect_test() ->
    %% 1. minecraft:apply_effects (Type 0)
    DetailInput = #potion_effect_detail{
        type = 'minecraft:potion_effect_detail',
        amplifier = 0,
        duration = 600,
        ambient = false,
        show_particles = true,
        show_icon = true,
        hidden_effect = none
    },
    DetailDecoded = #potion_effect_detail{
        type = 'minecraft:potion_effect_detail',
        amplifier = 0,
        duration = 600,
        ambient = false,
        show_particles = true,
        show_icon = true,
        hidden_effect = #prefixed_optional{some = none, prefixed_optional = none}
    },
    EffectInput = #potion_effect{
        type = 'minecraft:potion_effect',
        id = 'minecraft:speed',
        details = DetailInput
    },
    EffectDecoded = #potion_effect{
        type = 'minecraft:potion_effect',
        id = 'minecraft:speed',
        details = DetailDecoded
    },
    ApplyEffectRecordInput = #consume_effect{
        type = 'minecraft:consume_effect',
        effect_type = 'minecraft:apply_effects',
        effects = [EffectInput],
        probability = 1.0
    },
    ApplyEffectRecordDecoded = #consume_effect{
        type = 'minecraft:consume_effect',
        effect_type = 'minecraft:apply_effects',
        effects = [EffectDecoded],
        probability = 1.0
    },
    ApplyEnc = component_encode:encode_component('minecraft:consume_effect', ApplyEffectRecordInput),
    ?assertEqual({<<>>, ApplyEffectRecordDecoded}, component_decode:decode_component('minecraft:consume_effect', ApplyEnc)),
    ?assertEqual({<<>>, ApplyEffectRecordDecoded}, component_decode:decode_component(consume_effect, ApplyEnc)),

    %% Test map encoding for apply_effects
    ApplyMap = #{
        effect_type => 'minecraft:apply_effects',
        effects => [EffectInput],
        probability => 1.0
    },
    ?assertEqual(ApplyEnc, component_encode:encode_component('minecraft:consume_effect', ApplyMap)),

    %% 2. minecraft:remove_effects (Type 1)
    %% 2a. ID set as Tag Name String
    RemoveEffectTagRecordInput = #consume_effect{
        type = 'minecraft:consume_effect',
        effect_type = 'minecraft:remove_effects',
        effects = "minecraft:speed"
    },
    RemoveEffectTagRecordDecoded = #consume_effect{
        type = 'minecraft:consume_effect',
        effect_type = 'minecraft:remove_effects',
        effects = #id_set{id_set = "minecraft:speed"}
    },
    RemoveEncTag = component_encode:encode_component('minecraft:consume_effect', RemoveEffectTagRecordInput),
    ?assertEqual({<<>>, RemoveEffectTagRecordDecoded}, component_decode:decode_component('minecraft:consume_effect', RemoveEncTag)),

    %% 2b. ID set as integer list
    RemoveEffectIDsRecordInput = #consume_effect{
        type = 'minecraft:consume_effect',
        effect_type = 'minecraft:remove_effects',
        effects = [1]
    },
    RemoveEffectIDsRecordDecoded = #consume_effect{
        type = 'minecraft:consume_effect',
        effect_type = 'minecraft:remove_effects',
        effects = #id_set{id_set = [1]}
    },
    RemoveEncIDs = component_encode:encode_component('minecraft:consume_effect', RemoveEffectIDsRecordInput),
    ?assertEqual({<<>>, RemoveEffectIDsRecordDecoded}, component_decode:decode_component('minecraft:consume_effect', RemoveEncIDs)),

    %% 3. minecraft:clear_all_effects (Type 2)
    ClearEffectRecord = #consume_effect{
        type = 'minecraft:consume_effect',
        effect_type = 'minecraft:clear_all_effects'
    },
    ClearEnc = component_encode:encode_component('minecraft:consume_effect', ClearEffectRecord),
    ?assertEqual(<<2>>, ClearEnc),
    ?assertEqual({<<>>, ClearEffectRecord}, component_decode:decode_component('minecraft:consume_effect', ClearEnc)),

    %% 4. minecraft:teleport_randomly (Type 3)
    TeleportRecord = #consume_effect{
        type = 'minecraft:consume_effect',
        effect_type = 'minecraft:teleport_randomly',
        diameter = 16.0
    },
    TeleportEnc = component_encode:encode_component('minecraft:consume_effect', TeleportRecord),
    ?assertEqual({<<>>, TeleportRecord}, component_decode:decode_component('minecraft:consume_effect', TeleportEnc)),

    %% 5. minecraft:play_sound (Type 4)
    SoundRecordInput = #consume_effect{
        type = 'minecraft:consume_effect',
        effect_type = 'minecraft:play_sound',
        sound = {"minecraft:entity.generic.consume", false, undefined}
    },
    SoundRecordDecoded = #consume_effect{
        type = 'minecraft:consume_effect',
        effect_type = 'minecraft:play_sound',
        sound = #sound_event{sound_name = "minecraft:entity.generic.consume", has_fixed_value = false, fixed_range = undefined}
    },
    SoundEnc = component_encode:encode_component('minecraft:consume_effect', SoundRecordInput),
    ?assertEqual({<<>>, SoundRecordDecoded}, component_decode:decode_component('minecraft:consume_effect', SoundEnc)),

    %% Test generic encode/decode roundtrip via encode:encode_type / decode:decode_type
    EncGeneric = encode:encode_type(ClearEffectRecord, 'minecraft:consume_effect'),
    ?assertEqual({<<>>, ClearEffectRecord}, decode:decode_type(EncGeneric, 'minecraft:consume_effect')).

instrument_test() ->
    InlineSound = {"minecraft:item.goat_horn.play", false, undefined},
    InstrumentRecordInput = #instrument{
        type = 'minecraft:instrument',
        sound_event = InlineSound,
        use_duration = 7.0,
        range = 256.0,
        description = <<"\"Goat Horn\"">>
    },
    ExpectedDecodedDescription = #text_component{component_map = #{type => <<"text">>, text => <<"Goat Horn">>}},
    ExpectedDecodedRecord = #instrument{
        type = 'minecraft:instrument',
        sound_event = #id_or_x{id_or_x = #sound_event{sound_name = "minecraft:item.goat_horn.play", has_fixed_value = false, fixed_range = undefined}},
        use_duration = 7.0,
        range = 256.0,
        description = ExpectedDecodedDescription
    },
    %% Test sub-type encoding/decoding via component_encode / component_decode
    EncodedSub = component_encode:encode_component(instrument, InstrumentRecordInput),
    ?assertEqual({<<>>, ExpectedDecodedRecord}, component_decode:decode_component(instrument, EncodedSub)),

    %% Test map encoding
    InstrumentMap = #{
        sound_event => InlineSound,
        use_duration => 7.0,
        range => 256.0,
        description => <<"\"Goat Horn\"">>
    },

    ?assertEqual(EncodedSub, component_encode:encode_component(instrument, InstrumentMap)),

    %% Test component dispatch for 'minecraft:instrument' (ID 61)
    InstId = component_type_registry:name_to_id('minecraft:instrument'),
    ?assertEqual(61, InstId),

    %% Inline instrument via 'minecraft:instrument' component (id_or_x: tag 0 + inline instrument)
    EncodedComponent = component_encode:encode_component('minecraft:instrument', InstrumentRecordInput),
    ExpectedComponentDecoded = #id_or_x{id_or_x = ExpectedDecodedRecord},
    ?assertEqual({<<>>, ExpectedComponentDecoded}, component_decode:decode_component('minecraft:instrument', EncodedComponent)),
    ?assertEqual({<<>>, ExpectedComponentDecoded}, component_decode:decode_component(InstId, EncodedComponent)),

    %% Registry ID instrument reference (id_or_x: {id, 3})
    EncodedIdRef = component_encode:encode_component('minecraft:instrument', {id, 3}),
    ?assertEqual(<<4>>, EncodedIdRef),
    ?assertEqual({<<>>, #id_or_x{id_or_x = 3}}, component_decode:decode_component('minecraft:instrument', EncodedIdRef)),

    %% Generic encode/decode roundtrip
    EncGeneric = encode:encode_type(InstrumentRecordInput, instrument),
    ?assertEqual({<<>>, ExpectedDecodedRecord}, decode:decode_type(EncGeneric, instrument)).

jukebox_song_test() ->
    JukeboxSongRecordInput = #jukebox_song{
        type = 'minecraft:jukebox_song',
        sound_event = {id, 12},
        description = <<"Pigstep">>,
        duration = 148.0,
        output = 15
    },
    ExpectedDecodedDescription = #text_component{component_map = #{type => <<"text">>, text => <<"Pigstep">>}},
    ExpectedDecodedRecord = #jukebox_song{
        type = 'minecraft:jukebox_song',
        sound_event = #id_or_x{id_or_x = 12},
        description = ExpectedDecodedDescription,
        duration = 148.0,
        output = 15
    },
    %% Test sub-type encoding/decoding
    EncodedSub = component_encode:encode_component(jukebox_song, JukeboxSongRecordInput),
    ?assertEqual({<<>>, ExpectedDecodedRecord}, component_decode:decode_component(jukebox_song, EncodedSub)),

    %% Test map encoding
    JukeboxSongMap = #{
        sound_event => {id, 12},
        description => <<"Pigstep">>,
        duration => 148.0,
        output => 15
    },
    ?assertEqual(EncodedSub, component_encode:encode_component(jukebox_song, JukeboxSongMap)),

    %% Test component dispatch for 'minecraft:jukebox_playable' (ID 64)
    JukeboxPlayableId = component_type_registry:name_to_id('minecraft:jukebox_playable'),
    ?assertEqual(64, JukeboxPlayableId),

    EncodedComponent = component_encode:encode_component('minecraft:jukebox_playable', JukeboxSongRecordInput),
    ExpectedComponentDecoded = #id_or_x{id_or_x = ExpectedDecodedRecord},
    ?assertEqual({<<>>, ExpectedComponentDecoded}, component_decode:decode_component('minecraft:jukebox_playable', EncodedComponent)),
    ?assertEqual({<<>>, ExpectedComponentDecoded}, component_decode:decode_component(JukeboxPlayableId, EncodedComponent)),

    %% Registry ID jukebox song reference
    EncodedIdRef = component_encode:encode_component('minecraft:jukebox_playable', {id, 5}),
    ?assertEqual(<<6>>, EncodedIdRef),
    ?assertEqual({<<>>, #id_or_x{id_or_x = 5}}, component_decode:decode_component('minecraft:jukebox_playable', EncodedIdRef)),

    %% Generic encode/decode roundtrip
    EncGeneric = encode:encode_type(JukeboxSongRecordInput, jukebox_song),
    ?assertEqual({<<>>, ExpectedDecodedRecord}, decode:decode_type(EncGeneric, jukebox_song)).

banner_pattern_test() ->
    BannerPatternRecordInput = #banner_pattern{
        type = 'minecraft:banner_pattern',
        asset_id = "minecraft:flower",
        translation_key = "block.minecraft.banner.flower.red"
    },
    BannerPatternRecordDecoded = #banner_pattern{
        type = 'minecraft:banner_pattern',
        asset_id = #identifier{identifier = "minecraft:flower"},
        translation_key = "block.minecraft.banner.flower.red"
    },
    %% Test sub-type encoding/decoding
    EncodedSub = component_encode:encode_component(banner_pattern, BannerPatternRecordInput),
    ?assertEqual({<<>>, BannerPatternRecordDecoded}, component_decode:decode_component(banner_pattern, EncodedSub)),

    %% Test map encoding
    BannerPatternMap = #{
        asset_id => "minecraft:flower",
        translation_key => "block.minecraft.banner.flower.red"
    },
    ?assertEqual(EncodedSub, component_encode:encode_component(banner_pattern, BannerPatternMap)),

    %% Test 'minecraft:banner_pattern' atom dispatch
    ?assertEqual(EncodedSub, component_encode:encode_component('minecraft:banner_pattern', BannerPatternRecordInput)),
    ?assertEqual({<<>>, BannerPatternRecordDecoded}, component_decode:decode_component('minecraft:banner_pattern', EncodedSub)),

    %% Test banner_patterns component (ID 72)
    BannerPatternsComponentId = component_type_registry:name_to_id('minecraft:banner_patterns'),
    ?assertEqual(72, BannerPatternsComponentId),

    PatternList = [
        {{id, 0}, red},
        {{val, BannerPatternRecordInput}, blue}
    ],
    ExpectedDecodedList = #prefixed_array{prefixed_array = [
        {#id_or_x{id_or_x = 0}, #dye{type = 'minecraft:dye', colour = red}},
        {#id_or_x{id_or_x = BannerPatternRecordDecoded}, #dye{type = 'minecraft:dye', colour = blue}}
    ]},
    EncodedBannerPatterns = component_encode:encode_component('minecraft:banner_patterns', PatternList),
    ?assertEqual({<<>>, ExpectedDecodedList}, component_decode:decode_component('minecraft:banner_patterns', EncodedBannerPatterns)),
    ?assertEqual({<<>>, ExpectedDecodedList}, component_decode:decode_component(BannerPatternsComponentId, EncodedBannerPatterns)),

    %% Generic encode/decode roundtrip
    EncGeneric = encode:encode_type(BannerPatternRecordInput, banner_pattern),
    ?assertEqual({<<>>, BannerPatternRecordDecoded}, decode:decode_type(EncGeneric, banner_pattern)).

custom_data_test() ->
    CustomDataId = component_type_registry:name_to_id('minecraft:custom_data'),
    ?assertEqual(0, CustomDataId),

    NbtData = [{tag_compound, [], [{tag_string, "custom_key", "custom_val"}]}],
    ExpectedRecord = #custom_data{type = 'minecraft:custom_data', data = NbtData},

    EncodedRec = component_encode:encode_component('minecraft:custom_data', ExpectedRecord),
    EncodedMap = component_encode:encode_component('minecraft:custom_data', #{data => NbtData}),
    EncodedRaw = component_encode:encode_component('minecraft:custom_data', NbtData),
    ?assertEqual(EncodedRec, EncodedMap),
    ?assertEqual(EncodedRec, EncodedRaw),

    ?assertEqual(EncodedRec, component_encode:encode_component(custom_data, ExpectedRecord)),
    ?assertEqual(EncodedRec, component_encode:encode_component(CustomDataId, ExpectedRecord)),

    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component('minecraft:custom_data', EncodedRec)),
    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component(custom_data, EncodedRec)),
    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component(CustomDataId, EncodedRec)),

    EncGeneric = encode:encode_type(ExpectedRecord, 'minecraft:custom_data'),
    ?assertEqual(EncodedRec, EncGeneric),
    ?assertEqual({<<>>, ExpectedRecord}, decode:decode_type(EncGeneric, 'minecraft:custom_data')).

max_stack_size_test() ->
    MaxStackSizeId = component_type_registry:name_to_id('minecraft:max_stack_size'),
    ?assertEqual(1, MaxStackSizeId),

    StackSize = 64,
    ExpectedRecord = #max_stack_size{type = 'minecraft:max_stack_size', max_stack_size = StackSize},
    ExpectedBin = <<64>>,

    EncodedRec = component_encode:encode_component('minecraft:max_stack_size', ExpectedRecord),
    EncodedMap = component_encode:encode_component('minecraft:max_stack_size', #{max_stack_size => StackSize}),
    EncodedRaw = component_encode:encode_component('minecraft:max_stack_size', StackSize),
    ?assertEqual(ExpectedBin, EncodedRec),
    ?assertEqual(ExpectedBin, EncodedMap),
    ?assertEqual(ExpectedBin, EncodedRaw),

    ?assertEqual(ExpectedBin, component_encode:encode_component(max_stack_size, ExpectedRecord)),
    ?assertEqual(ExpectedBin, component_encode:encode_component(MaxStackSizeId, ExpectedRecord)),

    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component('minecraft:max_stack_size', ExpectedBin)),
    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component(max_stack_size, ExpectedBin)),
    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component(MaxStackSizeId, ExpectedBin)),

    EncGeneric = encode:encode_type(ExpectedRecord, 'minecraft:max_stack_size'),
    ?assertEqual(ExpectedBin, EncGeneric),
    ?assertEqual({<<>>, ExpectedRecord}, decode:decode_type(EncGeneric, 'minecraft:max_stack_size')).

max_damage_test() ->
    MaxDamageId = component_type_registry:name_to_id('minecraft:max_damage'),
    ?assertEqual(2, MaxDamageId),

    MaxDamageVal = 1561,
    ExpectedRecord = #max_damage{type = 'minecraft:max_damage', max_damage = MaxDamageVal},

    EncodedRec = component_encode:encode_component('minecraft:max_damage', ExpectedRecord),
    EncodedMap = component_encode:encode_component('minecraft:max_damage', #{max_damage => MaxDamageVal}),
    EncodedRaw = component_encode:encode_component('minecraft:max_damage', MaxDamageVal),
    ?assertEqual(EncodedRec, EncodedMap),
    ?assertEqual(EncodedRec, EncodedRaw),

    ?assertEqual(EncodedRec, component_encode:encode_component(max_damage, ExpectedRecord)),
    ?assertEqual(EncodedRec, component_encode:encode_component(MaxDamageId, ExpectedRecord)),

    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component('minecraft:max_damage', EncodedRec)),
    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component(max_damage, EncodedRec)),
    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component(MaxDamageId, EncodedRec)),

    EncGeneric = encode:encode_type(ExpectedRecord, 'minecraft:max_damage'),
    ?assertEqual(EncodedRec, EncGeneric),
    ?assertEqual({<<>>, ExpectedRecord}, decode:decode_type(EncGeneric, 'minecraft:max_damage')).

damage_test() ->
    DamageId = component_type_registry:name_to_id('minecraft:damage'),
    ?assertEqual(3, DamageId),

    DamageVal = 42,
    ExpectedRecord = #damage{type = 'minecraft:damage', damage = DamageVal},
    ExpectedBin = <<42>>,

    EncodedRec = component_encode:encode_component('minecraft:damage', ExpectedRecord),
    EncodedMap = component_encode:encode_component('minecraft:damage', #{damage => DamageVal}),
    EncodedRaw = component_encode:encode_component('minecraft:damage', DamageVal),
    ?assertEqual(ExpectedBin, EncodedRec),
    ?assertEqual(ExpectedBin, EncodedMap),
    ?assertEqual(ExpectedBin, EncodedRaw),

    ?assertEqual(ExpectedBin, component_encode:encode_component(damage, ExpectedRecord)),
    ?assertEqual(ExpectedBin, component_encode:encode_component(DamageId, ExpectedRecord)),

    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component('minecraft:damage', ExpectedBin)),
    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component(damage, ExpectedBin)),
    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component(DamageId, ExpectedBin)),

    EncGeneric = encode:encode_type(ExpectedRecord, 'minecraft:damage'),
    ?assertEqual(ExpectedBin, EncGeneric),
    ?assertEqual({<<>>, ExpectedRecord}, decode:decode_type(EncGeneric, 'minecraft:damage')).

unbreakable_test() ->
    UnbreakableId = component_type_registry:name_to_id('minecraft:unbreakable'),
    ?assertEqual(4, UnbreakableId),

    ExpectedRecord = #unbreakable{type = 'minecraft:unbreakable'},

    EncodedRec = component_encode:encode_component('minecraft:unbreakable', ExpectedRecord),
    EncodedMap = component_encode:encode_component('minecraft:unbreakable', #{}),
    ?assertEqual(<<>>, EncodedRec),
    ?assertEqual(<<>>, EncodedMap),

    ?assertEqual(<<>>, component_encode:encode_component(unbreakable, ExpectedRecord)),
    ?assertEqual(<<>>, component_encode:encode_component(UnbreakableId, ExpectedRecord)),

    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component('minecraft:unbreakable', <<>>)),
    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component(unbreakable, <<>>)),
    ?assertEqual({<<>>, ExpectedRecord}, component_decode:decode_component(UnbreakableId, <<>>)),

    EncGeneric = encode:encode_type(ExpectedRecord, 'minecraft:unbreakable'),
    ?assertEqual(<<>>, EncGeneric),
    ?assertEqual({<<>>, ExpectedRecord}, decode:decode_type(<<>>, 'minecraft:unbreakable')).
