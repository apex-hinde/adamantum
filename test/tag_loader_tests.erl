-module(tag_loader_tests).
-include("src/data_types/records.hrl").

-include_lib("eunit/include/eunit.hrl").
load_tags_test() ->
    Tags = tag_loader:load_all_tags(),
    ?assert(is_list(Tags)),
    ?assert(length(Tags) > 0),
    
    %% Verify structure of loaded tags: [[RegId, [[TagId, [EntryIds]]]]]
    [FirstReg | _] = Tags,
    ?assertMatch([_RegId, _TagList], FirstReg),
    [_RegId, TagList] = FirstReg,
    ?assert(is_list(TagList)),
    ?assert(length(TagList) > 0),
    [FirstTag | _] = TagList,
    ?assertMatch([_TagId, _EntryIds], FirstTag),
    [_TagId, EntryIds] = FirstTag,
    ?assert(is_list(EntryIds)).

encode_update_tags_test() ->
    Tags = tag_loader:load_all_tags(),
    Record = #'minecraft:update_tags'{tags = Tags},
    {EncodedMsg, _} = player:encode_message('minecraft:update_tags', Record, undefined),
    ?assertMatch({'minecraft:update_tags', [_]}, EncodedMsg),
    EncodedBin = encode:encode_message(EncodedMsg, 'minecraft:update_tags', 4),
    ?assert(is_binary(EncodedBin)),
    ?assert(byte_size(EncodedBin) > 0).
