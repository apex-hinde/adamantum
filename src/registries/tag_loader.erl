-module(tag_loader).

-export([
    load_all_tags/0
]).

%% @doc Load all tags from registry/tags/ directory and format for minecraft:update_tags packet.
-spec load_all_tags() -> list().
load_all_tags() ->
    RegMap = load_vanilla_registries_map(),
    TagsDir = "registry/tags",
    AllTagFiles = find_tag_files(TagsDir),
    ParsedTags = lists:filtermap(fun(FilePath) -> parse_tag_file(FilePath, TagsDir) end, AllTagFiles),

    %% Group tags by RegistryId
    TagsByRegistry = group_by_registry(ParsedTags),

    %% Filter to only allowed (synchronized or static) registries
    AllowedTagsByRegistry = lists:filter(fun({RegId, _}) -> is_allowed_registry(RegId) end, TagsByRegistry),

    %% Build tag map for tag reference (#tag) resolution per registry
    TagMapPerReg = build_tag_map_per_registry(AllowedTagsByRegistry),

    %% Resolve entry IDs for all tags in each registry
    lists:map(fun({RegId, TagList}) ->
        ResolvedTags = lists:map(fun({TagId, Values}) ->
            EntryIds = resolve_tag_values(RegId, Values, TagMapPerReg, RegMap, [TagId]),
            [TagId, EntryIds]
        end, TagList),
        [RegId, ResolvedTags]
    end, AllowedTagsByRegistry).

is_allowed_registry(RegId) ->
    StaticRegs = [
        "minecraft:item",
        "minecraft:block",
        "minecraft:entity_type",
        "minecraft:fluid",
        "minecraft:game_event",
        "minecraft:point_of_interest_type",
        "minecraft:potion"
    ],
    lists:member(RegId, StaticRegs) orelse lists:keymember(RegId, 1, registry_loader:required_registries()).

%% Find all .json files in registry/tags
find_tag_files(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:flatmap(fun(File) ->
                Path = filename:join(Dir, File),
                case filelib:is_dir(Path) of
                    true -> find_tag_files(Path);
                    false ->
                        case filename:extension(File) of
                            ".json" -> [Path];
                            _ -> []
                        end
                end
            end, Files);
        {error, _} ->
            []
    end.

%% Parse tag file path into {RegistryId, TagId, RawValues}
parse_tag_file(FilePath, BaseDir) ->
    RelPath = relative_path(FilePath, BaseDir),
    Parts = filename:split(RelPath),
    case Parts of
        ["worldgen", RegSub | TagPathParts] ->
            RegId = "minecraft:worldgen/" ++ RegSub,
            TagSubPath = filename:join(TagPathParts),
            TagId = "minecraft:" ++ filename:rootname(TagSubPath),
            read_values_from_json(FilePath, RegId, TagId);
        [RegSub | TagPathParts] when length(TagPathParts) > 0 ->
            RegId = "minecraft:" ++ RegSub,
            TagSubPath = filename:join(TagPathParts),
            TagId = "minecraft:" ++ filename:rootname(TagSubPath),
            read_values_from_json(FilePath, RegId, TagId);
        _ ->
            false
    end.

relative_path(Path, BaseDir) ->
    BaseLen = length(BaseDir),
    Sub = lists:nthtail(BaseLen, Path),
    case Sub of
        "/" ++ Rest -> Rest;
        _ -> Sub
    end.

read_values_from_json(FilePath, RegId, TagId) ->
    case file:read_file(FilePath) of
        {ok, Bin} ->
            try
                JsonMap = json:decode(Bin),
                RawValues = maps:get(<<"values">>, JsonMap, []),
                Values = [ extract_value_str(V) || V <- RawValues ],
                {true, {RegId, TagId, Values}}
            catch
                _:_ -> false
            end;
        _ ->
            false
    end.

extract_value_str(V) when is_binary(V) -> binary_to_list(V);
extract_value_str(V) when is_list(V) -> V;
extract_value_str(#{<<"id">> := Id}) when is_binary(Id) -> binary_to_list(Id);
extract_value_str(#{<<"id">> := Id}) when is_list(Id) -> Id;
extract_value_str(_) -> "".

group_by_registry(ParsedTags) ->
    Dict = lists:foldl(fun({RegId, TagId, Values}, Acc) ->
        dict:append(RegId, {TagId, Values}, Acc)
    end, dict:new(), ParsedTags),
    dict:to_list(Dict).

build_tag_map_per_registry(TagsByRegistry) ->
    lists:foldl(fun({RegId, TagList}, AccMap) ->
        RegTagMap = lists:foldl(fun({TagId, Values}, Acc) ->
            maps:put(TagId, Values, Acc)
        end, #{}, TagList),
        maps:put(RegId, RegTagMap, AccMap)
    end, #{}, TagsByRegistry).

%% Resolve list of value strings (entries or #tag refs) to sorted list of unique integer entry IDs
resolve_tag_values(RegId, Values, TagMapPerReg, VanillaRegMap, VisitedTags) ->
    RegTagMap = maps:get(RegId, TagMapPerReg, #{}),
    Ids = lists:flatmap(fun(Val) ->
        case Val of
            "#" ++ RefTagId ->
                FullRefId = case RefTagId of
                    "minecraft:" ++ _ -> RefTagId;
                    _ -> "minecraft:" ++ RefTagId
                end,
                case lists:member(FullRefId, VisitedTags) of
                    true -> []; %% Prevent infinite recursion
                    false ->
                        RefValues = maps:get(FullRefId, RegTagMap, []),
                        resolve_tag_values(RegId, RefValues, TagMapPerReg, VanillaRegMap, [FullRefId | VisitedTags])
                end;
            "" -> [];
            EntryName ->
                FullEntryName = case EntryName of
                    "minecraft:" ++ _ -> EntryName;
                    _ -> "minecraft:" ++ EntryName
                end,
                case resolve_entry_id(RegId, FullEntryName, VanillaRegMap) of
                    {ok, Id} -> [Id];
                    error -> []
                end
        end
    end, Values),
    lists:usort(Ids).

resolve_entry_id(RegId, EntryName, VanillaRegMap) ->
    %% 1. Check Vanilla Registries Map (json-26.2/registries.json)
    Key = {RegId, EntryName},
    case maps:find(Key, VanillaRegMap) of
        {ok, Id} -> {ok, Id};
        error ->
            %% 2. Check item_type_registry if RegId == "minecraft:item"
            case RegId of
                "minecraft:item" ->
                    try
                        Atom = list_to_atom(EntryName),
                        {ok, item_type_registry:name_to_id(Atom)}
                    catch
                        _:_ -> resolve_dynamic_registry_id(RegId, EntryName)
                    end;
                _ ->
                    resolve_dynamic_registry_id(RegId, EntryName)
            end
    end.

resolve_dynamic_registry_id(RegId, EntryName) ->
    Names = registry_loader:get_registry_entry_names(RegId),
    find_index(EntryName, Names, 0).

find_index(_Name, [], _Idx) -> error;
find_index(Name, [Name | _Rest], Idx) -> {ok, Idx};
find_index(Name, [_Other | Rest], Idx) -> find_index(Name, Rest, Idx + 1).

%% Load json-26.2/registries.json into map of {{RegIdStr, EntryIdStr}, ProtocolIdInt}
load_vanilla_registries_map() ->
    FilePath = "json-26.2/registries.json",
    case file:read_file(FilePath) of
        {ok, Bin} ->
            try
                JsonMap = json:decode(Bin),
                maps:fold(fun(RegKeyBin, RegData, Acc1) ->
                    RegIdStr = binary_to_list(RegKeyBin),
                    Entries = maps:get(<<"entries">>, RegData, #{}),
                    maps:fold(fun(EntryKeyBin, EntryData, Acc2) ->
                        EntryIdStr = binary_to_list(EntryKeyBin),
                        ProtoId = maps:get(<<"protocol_id">>, EntryData, 0),
                        maps:put({RegIdStr, EntryIdStr}, ProtoId, Acc2)
                    end, Acc1, Entries)
                end, #{}, JsonMap)
            catch
                _:_ -> #{}
            end;
        _ ->
            #{}
    end.
