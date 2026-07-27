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

text_and_identifier_components_test() ->
    %% custom_name, item_name, item_model, lore, rarity, tooltip_style, note_block_sound
    TextRec = #custom_name{type = 'minecraft:custom_name', name = #{type => <<"text">>, text => <<"Custom Name">>}},
    TextExpected = #custom_name{type = 'minecraft:custom_name', name = #text_component{component_map = #{type => <<"text">>, text => <<"Custom Name">>}}},
    EncText = component_encode:encode_component('minecraft:custom_name', TextRec),
    ?assertEqual({<<>>, TextExpected}, component_decode:decode_component('minecraft:custom_name', EncText)),

    ItemNameRec = #item_name{type = 'minecraft:item_name', name = #{type => <<"text">>, text => <<"Item Name">>}},
    ItemNameExp = #item_name{type = 'minecraft:item_name', name = #text_component{component_map = #{type => <<"text">>, text => <<"Item Name">>}}},
    EncItemName = component_encode:encode_component('minecraft:item_name', ItemNameRec),
    ?assertEqual({<<>>, ItemNameExp}, component_decode:decode_component('minecraft:item_name', EncItemName)),

    ItemModelRec = #item_model{type = 'minecraft:item_model', model = "minecraft:stick"},
    ItemModelExp = #item_model{type = 'minecraft:item_model', model = #identifier{identifier = "minecraft:stick"}},
    EncItemModel = component_encode:encode_component('minecraft:item_model', ItemModelRec),
    ?assertEqual({<<>>, ItemModelExp}, component_decode:decode_component('minecraft:item_model', EncItemModel)),

    LoreRec = #lore{type = 'minecraft:lore', lines = [#{type => <<"text">>, text => <<"Line 1">>}]},
    LoreExp = #lore{type = 'minecraft:lore', lines = [#text_component{component_map = #{type => <<"text">>, text => <<"Line 1">>}}]},
    EncLore = component_encode:encode_component('minecraft:lore', LoreRec),
    ?assertEqual({<<>>, LoreExp}, component_decode:decode_component('minecraft:lore', EncLore)),

    RarityRec = #rarity{type = 'minecraft:rarity', rarity = epic},
    RarityExp = #rarity{type = 'minecraft:rarity', rarity = epic},
    EncRarity = component_encode:encode_component('minecraft:rarity', RarityRec),
    ?assertEqual({<<>>, RarityExp}, component_decode:decode_component('minecraft:rarity', EncRarity)),

    TooltipStyleRec = #tooltip_style{type = 'minecraft:tooltip_style', style = "minecraft:special"},
    TooltipStyleExp = #tooltip_style{type = 'minecraft:tooltip_style', style = #identifier{identifier = "minecraft:special"}},
    EncStyle = component_encode:encode_component('minecraft:tooltip_style', TooltipStyleRec),
    ?assertEqual({<<>>, TooltipStyleExp}, component_decode:decode_component('minecraft:tooltip_style', EncStyle)),

    NoteBlockSoundRec = #note_block_sound{type = 'minecraft:note_block_sound', sound = "minecraft:block.bell.use"},
    NoteBlockSoundExp = #note_block_sound{type = 'minecraft:note_block_sound', sound = #identifier{identifier = "minecraft:block.bell.use"}},
    EncSound = component_encode:encode_component('minecraft:note_block_sound', NoteBlockSoundRec),
    ?assertEqual({<<>>, NoteBlockSoundExp}, component_decode:decode_component('minecraft:note_block_sound', EncSound)).

simple_scalar_components_test() ->
    RepairCostRec = #repair_cost{type = 'minecraft:repair_cost', cost = 5},
    EncRepairCost = component_encode:encode_component('minecraft:repair_cost', RepairCostRec),
    ?assertEqual({<<>>, RepairCostRec}, component_decode:decode_component('minecraft:repair_cost', EncRepairCost)),

    CreativeLockRec = #creative_slot_lock{type = 'minecraft:creative_slot_lock'},
    EncCreativeLock = component_encode:encode_component('minecraft:creative_slot_lock', CreativeLockRec),
    ?assertEqual({<<>>, CreativeLockRec}, component_decode:decode_component('minecraft:creative_slot_lock', EncCreativeLock)),

    GlintRec = #enchantment_glint_override{type = 'minecraft:enchantment_glint_override', has_glint = true},
    EncGlint = component_encode:encode_component('minecraft:enchantment_glint_override', GlintRec),
    ?assertEqual({<<>>, GlintRec}, component_decode:decode_component('minecraft:enchantment_glint_override', EncGlint)),

    GliderRec = #glider{type = 'minecraft:glider'},
    EncGlider = component_encode:encode_component('minecraft:glider', GliderRec),
    ?assertEqual({<<>>, GliderRec}, component_decode:decode_component('minecraft:glider', EncGlider)),

    EnchantableRec = #enchantable{type = 'minecraft:enchantable', value = 15},
    EncEnchantable = component_encode:encode_component('minecraft:enchantable', EnchantableRec),
    ?assertEqual({<<>>, EnchantableRec}, component_decode:decode_component('minecraft:enchantable', EncEnchantable)),

    MapIdRec = #map_id{type = 'minecraft:map_id', id = 42},
    EncMapId = component_encode:encode_component('minecraft:map_id', MapIdRec),
    ?assertEqual({<<>>, MapIdRec}, component_decode:decode_component('minecraft:map_id', EncMapId)),

    AmplifierRec = #ominous_bottle_amplifier{type = 'minecraft:ominous_bottle_amplifier', amplifier = 3},
    EncAmplifier = component_encode:encode_component('minecraft:ominous_bottle_amplifier', AmplifierRec),
    ?assertEqual({<<>>, AmplifierRec}, component_decode:decode_component('minecraft:ominous_bottle_amplifier', EncAmplifier)),

    ScaleRec = #potion_duration_scale{type = 'minecraft:potion_duration_scale', scale = 1.5},
    {<<>>, #potion_duration_scale{scale = ScaleDecoded}} = component_decode:decode_component('minecraft:potion_duration_scale', component_encode:encode_component('minecraft:potion_duration_scale', ScaleRec)),
    ?assert(abs(ScaleDecoded - 1.5) < 0.0001),

    TradeCostRec = #additional_trade_cost{type = 'minecraft:additional_trade_cost', cost = 2},
    EncTradeCost = component_encode:encode_component('minecraft:additional_trade_cost', TradeCostRec),
    ?assertEqual({<<>>, TradeCostRec}, component_decode:decode_component('minecraft:additional_trade_cost', EncTradeCost)),

    MinChargeRec = #minimum_attack_charge{type = 'minecraft:minimum_attack_charge', charge = 0.5},
    {<<>>, #minimum_attack_charge{charge = ChargeDecoded}} = component_decode:decode_component('minecraft:minimum_attack_charge', component_encode:encode_component('minecraft:minimum_attack_charge', MinChargeRec)),
    ?assert(abs(ChargeDecoded - 0.5) < 0.0001).

nbt_components_test() ->
    NbtData = [{tag_compound, [], [{tag_string, "key", "val"}]}],

    IntangibleRec = #intangible_projectile{type = 'minecraft:intangible_projectile', data = NbtData},
    EncIntangible = component_encode:encode_component('minecraft:intangible_projectile', IntangibleRec),
    ?assertEqual({<<>>, IntangibleRec}, component_decode:decode_component('minecraft:intangible_projectile', EncIntangible)),

    MapDecRec = #map_decorations{type = 'minecraft:map_decorations', data = NbtData},
    EncMapDec = component_encode:encode_component('minecraft:map_decorations', MapDecRec),
    ?assertEqual({<<>>, MapDecRec}, component_decode:decode_component('minecraft:map_decorations', EncMapDec)),

    DebugStickRec = #debug_stick_state{type = 'minecraft:debug_stick_state', data = NbtData},
    EncDebugStick = component_encode:encode_component('minecraft:debug_stick_state', DebugStickRec),
    ?assertEqual({<<>>, DebugStickRec}, component_decode:decode_component('minecraft:debug_stick_state', EncDebugStick)),

    BucketEntityRec = #bucket_entity_data{type = 'minecraft:bucket_entity_data', data = NbtData},
    EncBucketEntity = component_encode:encode_component('minecraft:bucket_entity_data', BucketEntityRec),
    ?assertEqual({<<>>, BucketEntityRec}, component_decode:decode_component('minecraft:bucket_entity_data', EncBucketEntity)),

    LockTag = [{tag_compound, [], [{tag_string, "KeyName", "val"}]}],
    LockRec = #lock{type = 'minecraft:lock', key = LockTag},
    EncLock = component_encode:encode_component('minecraft:lock', LockRec),
    ?assertEqual({<<>>, LockRec}, component_decode:decode_component('minecraft:lock', EncLock)),

    ContainerLootRec = #container_loot{type = 'minecraft:container_loot', data = NbtData},
    EncContainerLoot = component_encode:encode_component('minecraft:container_loot', ContainerLootRec),
    ?assertEqual({<<>>, ContainerLootRec}, component_decode:decode_component('minecraft:container_loot', EncContainerLoot)),

    RecipesRec = #recipes{type = 'minecraft:recipes', data = NbtData},
    EncRecipes = component_encode:encode_component('minecraft:recipes', RecipesRec),
    ?assertEqual({<<>>, RecipesRec}, component_decode:decode_component('minecraft:recipes', EncRecipes)).

color_and_map_components_test() ->
    DyedRec = #dyed_color{type = 'minecraft:dyed_color', color = 16711680},
    EncDyed = component_encode:encode_component('minecraft:dyed_color', DyedRec),
    ?assertEqual({<<>>, DyedRec}, component_decode:decode_component('minecraft:dyed_color', EncDyed)),

    MapColorRec = #map_color{type = 'minecraft:map_color', color = 65280},
    EncMapColor = component_encode:encode_component('minecraft:map_color', MapColorRec),
    ?assertEqual({<<>>, MapColorRec}, component_decode:decode_component('minecraft:map_color', EncMapColor)),

    PostProcRec = #map_post_processing{type = 'minecraft:map_post_processing', processing_type = lock},
    EncPostProc = component_encode:encode_component('minecraft:map_post_processing', PostProcRec),
    ?assertEqual({<<>>, PostProcRec}, component_decode:decode_component('minecraft:map_post_processing', EncPostProc)),

    BaseColorRec = #base_color{type = 'minecraft:base_color', color = red},
    EncBaseColor = component_encode:encode_component('minecraft:base_color', BaseColorRec),
    ?assertEqual({<<>>, #base_color{type = 'minecraft:base_color', color = red}}, component_decode:decode_component('minecraft:base_color', EncBaseColor)).

slot_components_test() ->
    EmptySlot = #slot{item_count = 0},
    DecodedEmptySlot = #slot{item_count = 0, components_to_add = [], components_to_remove = []},
    ChargedRec = #charged_projectiles{type = 'minecraft:charged_projectiles', projectiles = [EmptySlot]},
    ChargedExp = #charged_projectiles{type = 'minecraft:charged_projectiles', projectiles = [DecodedEmptySlot]},
    EncCharged = component_encode:encode_component('minecraft:charged_projectiles', ChargedRec),
    ?assertEqual({<<>>, ChargedExp}, component_decode:decode_component('minecraft:charged_projectiles', EncCharged)),

    BundleRec = #bundle_contents{type = 'minecraft:bundle_contents', items = [EmptySlot]},
    BundleExp = #bundle_contents{type = 'minecraft:bundle_contents', items = [DecodedEmptySlot]},
    EncBundle = component_encode:encode_component('minecraft:bundle_contents', BundleRec),
    ?assertEqual({<<>>, BundleExp}, component_decode:decode_component('minecraft:bundle_contents', EncBundle)),

    RemainderRec = #use_remainder{type = 'minecraft:use_remainder', remainder = EmptySlot},
    EncRemainder = component_encode:encode_component('minecraft:use_remainder', RemainderRec),
    ?assertEqual({<<>>, #use_remainder{type = 'minecraft:use_remainder', remainder = DecodedEmptySlot}}, component_decode:decode_component('minecraft:use_remainder', EncRemainder)),

    ContainerRec = #container{type = 'minecraft:container', items = [EmptySlot]},
    ContainerExp = #container{type = 'minecraft:container', items = [DecodedEmptySlot]},
    EncContainer = component_encode:encode_component('minecraft:container', ContainerRec),
    ?assertEqual({<<>>, ContainerExp}, component_decode:decode_component('minecraft:container', EncContainer)),

    SulfurRec = #sulfur_cube_content{type = 'minecraft:sulfur_cube_content', content = EmptySlot},
    EncSulfur = component_encode:encode_component('minecraft:sulfur_cube_content', SulfurRec),
    ?assertEqual({<<>>, #sulfur_cube_content{type = 'minecraft:sulfur_cube_content', content = DecodedEmptySlot}}, component_decode:decode_component('minecraft:sulfur_cube_content', EncSulfur)).

enchantment_components_test() ->
    EnchItem = {1, 3},
    EnchRec = #enchantments{type = 'minecraft:enchantments', enchantments = [EnchItem]},
    EnchExp = #enchantments{type = 'minecraft:enchantments', enchantments = [{#varint{varint = 1}, #varint{varint = 3}}]},
    EncEnch = component_encode:encode_component('minecraft:enchantments', EnchRec),
    ?assertEqual({<<>>, EnchExp}, component_decode:decode_component('minecraft:enchantments', EncEnch)),

    StoredRec = #stored_enchantments{type = 'minecraft:stored_enchantments', enchantments = [EnchItem]},
    StoredExp = #stored_enchantments{type = 'minecraft:stored_enchantments', enchantments = [{#varint{varint = 1}, #varint{varint = 3}}]},
    EncStored = component_encode:encode_component('minecraft:stored_enchantments', StoredRec),
    ?assertEqual({<<>>, StoredExp}, component_decode:decode_component('minecraft:stored_enchantments', EncStored)).

attribute_modifiers_test() ->
    Mod = {1, "minecraft:generic.attack_damage", 5.0, add, main_hand},
    Rec = #attribute_modifiers{type = 'minecraft:attribute_modifiers', modifiers = [Mod]},
    ModExp = {#varint{varint = 1}, #identifier{identifier = "minecraft:generic.attack_damage"}, #double{double = 5.0}, #enum{enum = add}, #enum{enum = main_hand}},
    Exp = #attribute_modifiers{type = 'minecraft:attribute_modifiers', modifiers = [ModExp]},
    Enc = component_encode:encode_component('minecraft:attribute_modifiers', Rec),
    ?assertEqual({<<>>, Exp}, component_decode:decode_component('minecraft:attribute_modifiers', Enc)).

custom_model_data_test() ->
    Rec = #custom_model_data{type = 'minecraft:custom_model_data', floats = [1.0], flags = [true], strings = ["test"], colors = [16711680]},
    Exp = #custom_model_data{type = 'minecraft:custom_model_data',
        floats = [#float{float = 1.0}],
        flags = [#bool{bool = true}],
        strings = [#string{string = "test"}],
        colors = [#int{int = 16711680}]
    },
    Enc = component_encode:encode_component('minecraft:custom_model_data', Rec),
    ?assertEqual({<<>>, Exp}, component_decode:decode_component('minecraft:custom_model_data', Enc)).

tooltip_display_test() ->
    Rec = #tooltip_display{type = 'minecraft:tooltip_display', hide_tooltip = false, hidden_components = [1]},
    Exp = #tooltip_display{type = 'minecraft:tooltip_display', hide_tooltip = false, hidden_components = [#varint{varint = 1}]},
    Enc = component_encode:encode_component('minecraft:tooltip_display', Rec),
    ?assertEqual({<<>>, Exp}, component_decode:decode_component('minecraft:tooltip_display', Enc)).

food_and_consumable_test() ->
    FoodRec = #food{type = 'minecraft:food', nutrition = 4, saturation_modifier = 0.6, can_always_eat = false},
    EncFood = component_encode:encode_component('minecraft:food', FoodRec),
    {<<>>, DecodedFood} = component_decode:decode_component('minecraft:food', EncFood),
    ?assertEqual('minecraft:food', DecodedFood#food.type),
    ?assertEqual(4, DecodedFood#food.nutrition),
    ?assert(abs(DecodedFood#food.saturation_modifier - 0.6) < 0.0001),
    ?assertEqual(false, DecodedFood#food.can_always_eat),

    ConsumableRec = #consumable{type = 'minecraft:consumable', consume_seconds = 1.6, animation = eat, sound = {id, 1}, has_consume_particles = true, effects = []},
    ConsumableExp = #consumable{type = 'minecraft:consumable', consume_seconds = 1.6, animation = eat, sound = #id_or_x{id_or_x = 1}, has_consume_particles = true, effects = []},
    EncConsumable = component_encode:encode_component('minecraft:consumable', ConsumableRec),
    {<<>>, DecodedConsumable} = component_decode:decode_component('minecraft:consumable', EncConsumable),
    ?assert(abs(DecodedConsumable#consumable.consume_seconds - 1.6) < 0.0001),
    ?assertEqual(ConsumableExp#consumable.animation, DecodedConsumable#consumable.animation),
    ?assertEqual(ConsumableExp#consumable.sound, DecodedConsumable#consumable.sound),
    ?assertEqual(ConsumableExp#consumable.has_consume_particles, DecodedConsumable#consumable.has_consume_particles),
    ?assertEqual(ConsumableExp#consumable.effects, DecodedConsumable#consumable.effects).

cooldown_and_use_effects_test() ->
    CooldownRec = #use_cooldown{type = 'minecraft:use_cooldown', seconds = 2.0, cooldown_group = {some, "minecraft:group"}},
    CooldownExp = #use_cooldown{type = 'minecraft:use_cooldown', seconds = 2.0, cooldown_group = #prefixed_optional{some = some, prefixed_optional = #identifier{identifier = "minecraft:group"}}},
    EncCooldown = component_encode:encode_component('minecraft:use_cooldown', CooldownRec),
    {<<>>, DecodedCooldown} = component_decode:decode_component('minecraft:use_cooldown', EncCooldown),
    ?assert(abs(DecodedCooldown#use_cooldown.seconds - 2.0) < 0.0001),
    ?assertEqual(CooldownExp#use_cooldown.cooldown_group, DecodedCooldown#use_cooldown.cooldown_group),

    EffectsRec = #use_effects{type = 'minecraft:use_effects', can_sprint = true, interact_vibrations = true, speed_multiplier = 1.2},
    EncEffects = component_encode:encode_component('minecraft:use_effects', EffectsRec),
    {<<>>, DecodedEffects} = component_decode:decode_component('minecraft:use_effects', EncEffects),
    ?assertEqual(true, DecodedEffects#use_effects.can_sprint),
    ?assertEqual(true, DecodedEffects#use_effects.interact_vibrations),
    ?assert(abs(DecodedEffects#use_effects.speed_multiplier - 1.2) < 0.0001).

damage_components_test() ->
    DmgTypeRec = #damage_type{type = 'minecraft:damage_type', damage_type = 1},
    EncDmgType = component_encode:encode_component('minecraft:damage_type', DmgTypeRec),
    ?assertEqual({<<>>, DmgTypeRec}, component_decode:decode_component('minecraft:damage_type', EncDmgType)),

    ResistantRec = #damage_resistant{type = 'minecraft:damage_resistant', types = "#minecraft:is_fire"},
    ResistantExp = #damage_resistant{type = 'minecraft:damage_resistant', types = #id_set{id_set = "#minecraft:is_fire"}},
    EncResistant = component_encode:encode_component('minecraft:damage_resistant', ResistantRec),
    ?assertEqual({<<>>, ResistantExp}, component_decode:decode_component('minecraft:damage_resistant', EncResistant)).

attack_and_weapon_components_test() ->
    AttackRangeRec = #attack_range{type = 'minecraft:attack_range', min_reach = 0.0, max_reach = 3.0, min_creative_reach = 0.0, max_creative_reach = 5.0, hitbox_margin = 0.5, mob_factor = 1.0},
    EncAttackRange = component_encode:encode_component('minecraft:attack_range', AttackRangeRec),
    ?assertEqual({<<>>, AttackRangeRec}, component_decode:decode_component('minecraft:attack_range', EncAttackRange)),

    WeaponRec = #weapon{type = 'minecraft:weapon', damage_per_attack = 5, disable_blocking_for = 1.5},
    EncWeapon = component_encode:encode_component('minecraft:weapon', WeaponRec),
    {<<>>, DecodedWeapon} = component_decode:decode_component('minecraft:weapon', EncWeapon),
    ?assertEqual(5, DecodedWeapon#weapon.damage_per_attack),
    ?assert(abs(DecodedWeapon#weapon.disable_blocking_for - 1.5) < 0.0001),

    SoundEvt = #sound_event{sound_name = "minecraft:item.pierce", has_fixed_value = false, fixed_range = undefined},
    PiercingRec = #piercing_weapon{type = 'minecraft:piercing_weapon', deals_knockback = true, dismounts = true, sound = {some, SoundEvt}, hit_sound = none},
    PiercingExp = #piercing_weapon{type = 'minecraft:piercing_weapon', deals_knockback = true, dismounts = true, sound = #prefixed_optional{some = some, prefixed_optional = SoundEvt}, hit_sound = #prefixed_optional{some = none, prefixed_optional = none}},
    EncPiercing = component_encode:encode_component('minecraft:piercing_weapon', PiercingRec),
    ?assertEqual({<<>>, PiercingExp}, component_decode:decode_component('minecraft:piercing_weapon', EncPiercing)),

    KineticRec = #kinetic_weapon{type = 'minecraft:kinetic_weapon', contact_cooldown_ticks = 10, delay_ticks = 5, dismount_conditions = none, knockback_conditions = none, damage_conditions = none, forward_movement = 1.0, damage_multiplier = 2.0, sound = none, hit_sound = none},
    KineticExp = #kinetic_weapon{type = 'minecraft:kinetic_weapon', contact_cooldown_ticks = 10, delay_ticks = 5, dismount_conditions = #prefixed_optional{some = none, prefixed_optional = none}, knockback_conditions = #prefixed_optional{some = none, prefixed_optional = none}, damage_conditions = #prefixed_optional{some = none, prefixed_optional = none}, forward_movement = 1.0, damage_multiplier = 2.0, sound = #prefixed_optional{some = none, prefixed_optional = none}, hit_sound = #prefixed_optional{some = none, prefixed_optional = none}},
    EncKinetic = component_encode:encode_component('minecraft:kinetic_weapon', KineticRec),
    ?assertEqual({<<>>, KineticExp}, component_decode:decode_component('minecraft:kinetic_weapon', EncKinetic)),

    SwingRec = #swing_animation{type = 'minecraft:swing_animation', animation_type = stab, duration = 10},
    EncSwing = component_encode:encode_component('minecraft:swing_animation', SwingRec),
    ?assertEqual({<<>>, SwingRec}, component_decode:decode_component('minecraft:swing_animation', EncSwing)).

tool_component_test() ->
    Rule = {"#minecraft:mineable/pickaxe", {some, 8.0}, {some, true}},
    RuleExp = {#id_set{id_set = "#minecraft:mineable/pickaxe"}, #prefixed_optional{some = some, prefixed_optional = #float{float = 8.0}}, #prefixed_optional{some = some, prefixed_optional = #bool{bool = true}}},
    ToolRec = #tool{type = 'minecraft:tool', rules = [Rule], default_mining_speed = 1.0, damage_per_block = 1, can_destroy_blocks_in_creative = true},
    ToolExp = #tool{type = 'minecraft:tool', rules = [RuleExp], default_mining_speed = 1.0, damage_per_block = 1, can_destroy_blocks_in_creative = true},
    EncTool = component_encode:encode_component('minecraft:tool', ToolRec),
    ?assertEqual({<<>>, ToolExp}, component_decode:decode_component('minecraft:tool', EncTool)).

equippable_and_repairable_test() ->
    EquipRec = #equippable{type = 'minecraft:equippable', slot = head, equip_sound = {id, 1}, model = none, camera_overlay = none, allowed_entities = none, dispensable = true, swappable = true, damage_on_hurt = true, can_be_sheared = false, shearing_sound = {id, 2}},
    EquipExp = #equippable{type = 'minecraft:equippable', slot = head, equip_sound = #id_or_x{id_or_x = 1}, model = #prefixed_optional{some = none, prefixed_optional = none}, camera_overlay = #prefixed_optional{some = none, prefixed_optional = none}, allowed_entities = #prefixed_optional{some = none, prefixed_optional = none}, dispensable = true, swappable = true, damage_on_hurt = true, can_be_sheared = false, shearing_sound = #id_or_x{id_or_x = 2}},
    EncEquip = component_encode:encode_component('minecraft:equippable', EquipRec),
    ?assertEqual({<<>>, EquipExp}, component_decode:decode_component('minecraft:equippable', EncEquip)),

    RepairRec = #repairable{type = 'minecraft:repairable', items = [1, 2]},
    RepairExp = #repairable{type = 'minecraft:repairable', items = #id_set{id_set = [1, 2]}},
    EncRepair = component_encode:encode_component('minecraft:repairable', RepairRec),
    ?assertEqual({<<>>, RepairExp}, component_decode:decode_component('minecraft:repairable', EncRepair)).

death_protection_and_blocks_attacks_test() ->
    DeathRec = #death_protection{type = 'minecraft:death_protection', effects = []},
    DeathExp = #death_protection{type = 'minecraft:death_protection', effects = []},
    EncDeath = component_encode:encode_component('minecraft:death_protection', DeathRec),
    ?assertEqual({<<>>, DeathExp}, component_decode:decode_component('minecraft:death_protection', EncDeath)),

    BlocksRec = #blocks_attacks{type = 'minecraft:blocks_attacks', block_delay_seconds = 0.0, disable_cooldown_scale = 1.0, damage_reductions = [{90.0, none, 0.0, 1.0}], bypassed_by = none, block_sound = none, disable_sound = none},
    BlocksExp = #blocks_attacks{type = 'minecraft:blocks_attacks', block_delay_seconds = 0.0, disable_cooldown_scale = 1.0, damage_reductions = #{reductions => [{#float{float = 90.0}, #prefixed_optional{some = none, prefixed_optional = none}, #float{float = 0.0}, #float{float = 1.0}}], item_damage_threshold => 0.0, item_damage_base => 0.0, item_damage_factor => 0.0}, bypassed_by = #prefixed_optional{some = none, prefixed_optional = none}, block_sound = #prefixed_optional{some = none, prefixed_optional = none}, disable_sound = #prefixed_optional{some = none, prefixed_optional = none}},
    EncBlocks = component_encode:encode_component('minecraft:blocks_attacks', BlocksRec),
    ?assertEqual({<<>>, BlocksExp}, component_decode:decode_component('minecraft:blocks_attacks', EncBlocks)).

potion_contents_and_stew_test() ->
    PotionRec = #potion_contents{type = 'minecraft:potion_contents', potion_id = {some, 1}, custom_color = none, custom_effects = [], custom_name = none},
    PotionExp = #potion_contents{type = 'minecraft:potion_contents', potion_id = #prefixed_optional{some = some, prefixed_optional = #varint{varint = 1}}, custom_color = #prefixed_optional{some = none, prefixed_optional = none}, custom_effects = [], custom_name = #prefixed_optional{some = none, prefixed_optional = none}},
    EncPotion = component_encode:encode_component('minecraft:potion_contents', PotionRec),
    ?assertEqual({<<>>, PotionExp}, component_decode:decode_component('minecraft:potion_contents', EncPotion)),

    StewRec = #suspicious_stew_effects{type = 'minecraft:suspicious_stew_effects', effects = [{1, 160}]},
    StewExp = #suspicious_stew_effects{type = 'minecraft:suspicious_stew_effects', effects = [{#varint{varint = 1}, #varint{varint = 160}}]},
    EncStew = component_encode:encode_component('minecraft:suspicious_stew_effects', StewRec),
    ?assertEqual({<<>>, StewExp}, component_decode:decode_component('minecraft:suspicious_stew_effects', EncStew)).

books_entity_data_and_fireworks_test() ->
    WritableRec = #writable_book_content{type = 'minecraft:writable_book_content', pages = [{"Page 1", none}]},
    WritableExp = #writable_book_content{type = 'minecraft:writable_book_content', pages = [{#string{string = "Page 1"}, #prefixed_optional{some = none, prefixed_optional = none}}]},
    EncWritable = component_encode:encode_component('minecraft:writable_book_content', WritableRec),
    ?assertEqual({<<>>, WritableExp}, component_decode:decode_component('minecraft:writable_book_content', EncWritable)),

    WrittenRec = #written_book_content{type = 'minecraft:written_book_content', raw_title = "Title", filtered_title = none, author = "Author", generation = 0, pages = [{#{type => <<"text">>, text => <<"Page 1">>}, none}], resolved = true},
    WrittenExp = #written_book_content{type = 'minecraft:written_book_content', raw_title = "Title", filtered_title = #prefixed_optional{some = none, prefixed_optional = none}, author = "Author", generation = 0, pages = [{#text_component{component_map = #{type => <<"text">>, text => <<"Page 1">>}}, #prefixed_optional{some = none, prefixed_optional = none}}], resolved = true},
    EncWritten = component_encode:encode_component('minecraft:written_book_content', WrittenRec),
    ?assertEqual({<<>>, WrittenExp}, component_decode:decode_component('minecraft:written_book_content', EncWritten)),

    NbtData = [{tag_compound, [], [{tag_string, "key", "val"}]}],
    EntityRec = #entity_data{type = 'minecraft:entity_data', entity_type = 1, data = NbtData},
    EncEntity = component_encode:encode_component('minecraft:entity_data', EntityRec),
    ?assertEqual({<<>>, EntityRec}, component_decode:decode_component('minecraft:entity_data', EncEntity)),

    BlockEntityRec = #block_entity_data{type = 'minecraft:block_entity_data', block_entity_type = 2, data = NbtData},
    EncBlockEntity = component_encode:encode_component('minecraft:block_entity_data', BlockEntityRec),
    ?assertEqual({<<>>, BlockEntityRec}, component_decode:decode_component('minecraft:block_entity_data', EncBlockEntity)),

    TrimMatRec = #provides_trim_material{type = 'minecraft:provides_trim_material', key = [1]},
    TrimMatExp = #provides_trim_material{type = 'minecraft:provides_trim_material', key = #id_set{id_set = [1]}},
    EncTrimMat = component_encode:encode_component('minecraft:provides_trim_material', TrimMatRec),
    ?assertEqual({<<>>, TrimMatExp}, component_decode:decode_component('minecraft:provides_trim_material', EncTrimMat)),

    FireworksRec = #fireworks{type = 'minecraft:fireworks', flight_duration = 1, explosions = []},
    FireworksExp = #fireworks{type = 'minecraft:fireworks', flight_duration = 1, explosions = []},
    EncFireworks = component_encode:encode_component('minecraft:fireworks', FireworksRec),
    ?assertEqual({<<>>, FireworksExp}, component_decode:decode_component('minecraft:fireworks', EncFireworks)).

profile_lodestone_and_misc_test() ->
    ProfileKind = 0,
    ProfileData = #prefixed_optional{some = some, prefixed_optional = #string{string = "Name"}},
    UuidData = #prefixed_optional{some = some, prefixed_optional = #uuid{uuid = <<0:128>>}},
    PropertiesData = [],
    ProfileRecordInput = {ProfileKind, {ProfileData, UuidData, PropertiesData}, none, none, none, none},
    ProfileRecordExpected = #resolvable_profile{
        profile_kind = ProfileKind,
        profile = {ProfileData, UuidData, PropertiesData},
        body = #optional{some = none, optional = none},
        cape = #optional{some = none, optional = none},
        elytra = #optional{some = none, optional = none},
        model = #optional{some = none, optional = none}
    },
    ProfileRec = #profile_component{type = 'minecraft:profile', profile = ProfileRecordInput},
    EncProfile = component_encode:encode_component('minecraft:profile', ProfileRec),
    ?assertEqual({<<>>, #profile_component{type = 'minecraft:profile', profile = ProfileRecordExpected}}, component_decode:decode_component('minecraft:profile', EncProfile)),

    LodestoneRec = #lodestone_tracker{type = 'minecraft:lodestone_tracker', has_global_position = false, dimension = undefined, position = undefined, tracked = true},
    EncLodestone = component_encode:encode_component('minecraft:lodestone_tracker', LodestoneRec),
    ?assertEqual({<<>>, LodestoneRec}, component_decode:decode_component('minecraft:lodestone_tracker', EncLodestone)),

    PotRec = #pot_decorations{type = 'minecraft:pot_decorations', decorations = [0, 0, 0, 0]},
    PotExp = #pot_decorations{type = 'minecraft:pot_decorations', decorations = [{varint, 0}, {varint, 0}, {varint, 0}, {varint, 0}]},
    EncPot = component_encode:encode_component('minecraft:pot_decorations', PotRec),
    ?assertEqual({<<>>, PotExp}, component_decode:decode_component('minecraft:pot_decorations', EncPot)),

    StateRec = #block_state{type = 'minecraft:block_state', properties = [{"facing", "north"}]},
    StateExp = #block_state{type = 'minecraft:block_state', properties = [{#string{string = "facing"}, #string{string = "north"}}]},
    EncState = component_encode:encode_component('minecraft:block_state', StateRec),
    ?assertEqual({<<>>, StateExp}, component_decode:decode_component('minecraft:block_state', EncState)),

    NbtCompoundData = [{tag_compound, "", [{tag_string, "name", "bee"}]}],
    BeesRec = #bees{type = 'minecraft:bees', bees = [{1, NbtCompoundData, 100, 200}]},
    BeesExp = #bees{type = 'minecraft:bees', bees = [{#varint{varint = 1}, #nbt{nbt = NbtCompoundData}, #varint{varint = 100}, #varint{varint = 200}}]},
    EncBees = component_encode:encode_component('minecraft:bees', BeesRec),
    ?assertEqual({<<>>, BeesExp}, component_decode:decode_component('minecraft:bees', EncBees)),

    BreakSoundRec = #break_sound{type = 'minecraft:break_sound', sound_event = {id, 5}},
    BreakSoundExp = #break_sound{type = 'minecraft:break_sound', sound_event = #id_or_x{id_or_x = 5}},
    EncBreakSound = component_encode:encode_component('minecraft:break_sound', BreakSoundRec),
    ?assertEqual({<<>>, BreakSoundExp}, component_decode:decode_component('minecraft:break_sound', EncBreakSound)),

    VariantRec = #variant_component{type = 'minecraft:fox/variant', variant = 1},
    EncVariant = component_encode:encode_component('minecraft:fox/variant', VariantRec),
    ?assertEqual({<<>>, VariantRec}, component_decode:decode_component('minecraft:fox/variant', EncVariant)).
