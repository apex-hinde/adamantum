-module(text_component_tests).
-include_lib("eunit/include/eunit.hrl").

string_shorthand_test() ->
    SNBT = <<"\"Hello world\"">>,
    Comp = text_component:decode(SNBT),
    ?assertEqual(<<"text">>, maps:get(type, Comp)),
    ?assertEqual(<<"Hello world">>, maps:get(text, Comp)).

list_shorthand_test() ->
    SNBT = <<"[\"A\", \"B\", \"C\"]">>,
    Comp = text_component:decode(SNBT),
    ?assertEqual(<<"text">>, maps:get(type, Comp)),
    ?assertEqual(<<"A">>, maps:get(text, Comp)),
    Extra = maps:get(extra, Comp),
    ?assertEqual(2, length(Extra)),
    [E1, E2] = Extra,
    ?assertEqual(<<"B">>, maps:get(text, E1)),
    ?assertEqual(<<"C">>, maps:get(text, E2)).

auto_type_inference_test() ->
    TextComp = text_component:decode(<<"{text: \"Hi\"}">>),
    ?assertEqual(<<"text">>, maps:get(type, TextComp)),

    TransComp = text_component:decode(<<"{translate: \"chat.type.text\"}">>),
    ?assertEqual(<<"translatable">>, maps:get(type, TransComp)),

    ScoreComp = text_component:decode(<<"{score: {name: \"@p\", objective: \"obj\"}}">>),
    ?assertEqual(<<"score">>, maps:get(type, ScoreComp)),

    SelComp = text_component:decode(<<"{selector: \"@a\"}">>),
    ?assertEqual(<<"selector">>, maps:get(type, SelComp)),

    KbComp = text_component:decode(<<"{keybind: \"key.inventory\"}">>),
    ?assertEqual(<<"keybind">>, maps:get(type, KbComp)),

    NbtComp = text_component:decode(<<"{nbt: \"CustomName\", entity: \"@s\"}">>),
    ?assertEqual(<<"nbt">>, maps:get(type, NbtComp)),

    ObjComp = text_component:decode(<<"{sprite: \"block/emerald_block\"}">>),
    ?assertEqual(<<"object">>, maps:get(type, ObjComp)).

translatable_test() ->
    SNBT = <<"{translate: \"chat.type.text\", fallback: \"Default\", with: [\"Steve\", \"Hello\"]}">>,
    Comp = text_component:decode(SNBT),
    ?assertEqual(<<"translatable">>, maps:get(type, Comp)),
    ?assertEqual(<<"chat.type.text">>, maps:get(translate, Comp)),
    ?assertEqual(<<"Default">>, maps:get(fallback, Comp)),
    With = maps:get(with, Comp),
    ?assertEqual(2, length(With)),
    [W1, W2] = With,
    ?assertEqual(<<"Steve">>, maps:get(text, W1)),
    ?assertEqual(<<"Hello">>, maps:get(text, W2)).

score_test() ->
    SNBT = <<"{score: {name: \"*\", objective: \"points\"}}">>,
    Comp = text_component:decode(SNBT),
    ?assertEqual(<<"score">>, maps:get(type, Comp)),
    Score = maps:get(score, Comp),
    ?assertEqual(<<"*">>, maps:get(name, Score)),
    ?assertEqual(<<"points">>, maps:get(objective, Score)).

selector_test() ->
    SNBT = <<"{selector: \"@e[type=cow]\", separator: {text: \"; \", color: \"red\"}}">>,
    Comp = text_component:decode(SNBT),
    ?assertEqual(<<"selector">>, maps:get(type, Comp)),
    ?assertEqual(<<"@e[type=cow]">>, maps:get(selector, Comp)),
    Sep = maps:get(separator, Comp),
    ?assertEqual(<<"; ">>, maps:get(text, Sep)),
    ?assertEqual(<<"red">>, maps:get(color, Sep)).

formatting_test() ->
    SNBT = <<"{text: \"Styled\", color: \"#FFAA00\", bold: 1b, italic: 0b, font: \"minecraft:alt\"}">>,
    Comp = text_component:decode(SNBT),
    ?assertEqual(<<"#FFAA00">>, maps:get(color, Comp)),
    ?assertEqual(<<"minecraft:alt">>, maps:get(font, Comp)),
    ?assertEqual(true, maps:get(bold, Comp)),
    ?assertEqual(false, maps:get(italic, Comp)).

shadow_color_test() ->
    IntComp = text_component:decode(<<"{text: \"Shadow\", shadow_color: 16711680}">>),
    ?assertEqual(16711680, maps:get(shadow_color, IntComp)),

    FloatComp = text_component:decode(<<"{text: \"Shadow\", shadow_color: [1.0f, 0.0f, 0.0f, 0.5f]}">>),
    %% Alpha=127<<24 + Red=255<<16 + Green=0 + Blue=0 = 2147418112
    Shadow = maps:get(shadow_color, FloatComp),
    ?assert(is_integer(Shadow)).

interactivity_test() ->
    SNBT = <<"{text: \"Click me\", insertion: \"shift_text\", click_event: {action: \"run_command\", command: \"/help\"}, hover_event: {action: \"show_text\", value: \"Tooltip\"}}">>,
    Comp = text_component:decode(SNBT),
    ?assertEqual(<<"shift_text">>, maps:get(insertion, Comp)),
    CE = maps:get(click_event, Comp),
    ?assertEqual(<<"run_command">>, maps:get(action, CE)),
    ?assertEqual(<<"/help">>, maps:get(command, CE)),
    HE = maps:get(hover_event, Comp),
    ?assertEqual(<<"show_text">>, maps:get(action, HE)),
    TooltipVal = maps:get(value, HE),
    ?assertEqual(<<"Tooltip">>, maps:get(text, TooltipVal)).

encode_decode_roundtrip_test() ->
    OriginalMap = #{
        type => <<"text">>,
        text => <<"Roundtrip Test">>,
        color => <<"gold">>,
        bold => true
    },
    SNBTBin = text_component:encode(OriginalMap),
    ?assert(is_binary(SNBTBin)),
    DecodedMap = text_component:decode(SNBTBin),
    ?assertEqual(<<"text">>, maps:get(type, DecodedMap)),
    ?assertEqual(<<"Roundtrip Test">>, maps:get(text, DecodedMap)),
    ?assertEqual(<<"gold">>, maps:get(color, DecodedMap)),
    ?assertEqual(true, maps:get(bold, DecodedMap)).

decode_type_integration_test() ->
    SNBTBin = <<"{text: \"Integration\"}">>,
    %% Length prefixed string format in decode_type
    LenBin = encode:encode_type(byte_size(SNBTBin), varint),
    InputBin = <<LenBin/binary, SNBTBin/binary, "rest">>,
    {Rest, Comp} = decode:decode_type(InputBin, text_component),
    ?assertEqual(<<"rest">>, Rest),
    ?assertEqual(<<"text">>, maps:get(type, Comp)),
    ?assertEqual(<<"Integration">>, maps:get(text, Comp)).

encode_type_integration_test() ->
    CompMap = #{type => <<"text">>, text => <<"Encoded">>},
    EncBin = encode:encode_type(CompMap, text_component),
    ?assert(is_binary(EncBin)),
    {Rest, DecodedComp} = decode:decode_type(EncBin, text_component),
    ?assertEqual(<<>>, Rest),
    ?assertEqual(<<"text">>, maps:get(type, DecodedComp)),
    ?assertEqual(<<"Encoded">>, maps:get(text, DecodedComp)).
