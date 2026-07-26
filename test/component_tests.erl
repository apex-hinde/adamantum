-module(component_tests).
-include_lib("eunit/include/eunit.hrl").

dye_color_sub_type_test() ->
    ?assertEqual(<<0>>, component:encode_dye_color(white)),
    ?assertEqual(<<14>>, component:encode_dye_color(red)),
    ?assertEqual(<<15>>, component:encode_dye_color(black)),
    ?assertEqual(<<14>>, component:encode_dye_color(14)),
    ?assertEqual({<<>>, white}, component:decode_dye_color(<<0>>)),
    ?assertEqual({<<>>, red}, component:decode_dye_color(<<14>>)),
    ?assertEqual({<<>>, black}, component:decode_dye_color(<<15>>)),
    ?assertEqual({error, "invalid enum value"}, component:decode_dye_color(<<16>>)).

dye_color_component_dispatch_test() ->
    %% minecraft:dye ID is 43
    DyeId = component_type_registry:name_to_id('minecraft:dye'),
    ?assertEqual(43, DyeId),
    ?assertEqual(<<14>>, component:encode_component(DyeId, red)),
    ?assertEqual(<<14>>, component:encode_component('minecraft:dye', red)),
    ?assertEqual({<<>>, red}, component:decode_component(DyeId, <<14>>)),
    ?assertEqual({<<>>, red}, component:decode_component('minecraft:dye', <<14>>)),

    %% minecraft:sheep/color ID is 109
    SheepColorId = component_type_registry:name_to_id('minecraft:sheep/color'),
    ?assertEqual(109, SheepColorId),
    ?assertEqual(<<0>>, component:encode_component(SheepColorId, white)),
    ?assertEqual({<<>>, white}, component:decode_component(SheepColorId, <<0>>)).

painting_variant_sub_type_test() ->
    %% ID variant (Tag 0)
    EncodedId = component:encode_painting_variant(5),
    ?assertEqual(<<0, 5>>, EncodedId),
    ?assertEqual({<<>>, {id, 5}}, component:decode_painting_variant(EncodedId)),

    %% Inline variant (Tag 1)
    VariantMap = #{
        asset_id => "minecraft:kebab",
        width => 1,
        height => 1,
        title => "\"Kebab\"",
        author => "\"Kristoffer Zetterstrand\""
    },
    EncodedInline = component:encode_painting_variant(VariantMap),
    ?assertEqual({<<>>, {inline, VariantMap}}, component:decode_painting_variant(EncodedInline)).

painting_variant_dispatch_test() ->
    PaintingVariantId = component_type_registry:name_to_id('minecraft:painting/variant'),
    ?assertEqual(103, PaintingVariantId),
    Encoded = component:encode_component(PaintingVariantId, 3),
    ?assertEqual(<<0, 3>>, Encoded),
    ?assertEqual({<<>>, {id, 3}}, component:decode_component(PaintingVariantId, Encoded)),
    ?assertEqual({<<>>, {id, 3}}, component:decode_component('minecraft:painting/variant', Encoded)).

