-module(registry_loader).
-include("src/data_types/records.hrl").

-export([
    required_registries/0,
    load_all_registries/0,
    load_registry/2,
    get_registry_entry_names/1,
    send_all_registries/1
]).


-spec required_registries() -> [{string(), string()}].
required_registries() ->
    [
        {"minecraft:banner_pattern", "registry/banner_pattern"},
        {"minecraft:cat_sound_variant", "registry/cat_sound_variant"},
        {"minecraft:cat_variant", "registry/cat_variant"},
        {"minecraft:chat_type", "registry/chat_type"},
        {"minecraft:chicken_sound_variant", "registry/chicken_sound_variant"},
        {"minecraft:chicken_variant", "registry/chicken_variant"},
        {"minecraft:cow_sound_variant", "registry/cow_sound_variant"},
        {"minecraft:cow_variant", "registry/cow_variant"},
        {"minecraft:damage_type", "registry/damage_type"},
        {"minecraft:dialog", "registry/dialog"},
        {"minecraft:dimension_type", "registry/dimension_type"},
        {"minecraft:enchantment", "registry/enchantment"},
        {"minecraft:frog_variant", "registry/frog_variant"},
        {"minecraft:instrument", "registry/instrument"},
        {"minecraft:jukebox_song", "registry/jukebox_song"},
        {"minecraft:painting_variant", "registry/painting_variant"},
        {"minecraft:pig_sound_variant", "registry/pig_sound_variant"},
        {"minecraft:pig_variant", "registry/pig_variant"},
        {"minecraft:sulfur_cube_archetype", "registry/sulfur_cube_archetype"},
        {"minecraft:test_environment", "registry/test_environment"},
        {"minecraft:test_instance", "registry/test_instance"},
        {"minecraft:timeline", "registry/timeline"},
        {"minecraft:trim_material", "registry/trim_material"},
        {"minecraft:trim_pattern", "registry/trim_pattern"},
        {"minecraft:wolf_sound_variant", "registry/wolf_sound_variant"},
        {"minecraft:wolf_variant", "registry/wolf_variant"},
        {"minecraft:world_clock", "registry/world_clock"},
        {"minecraft:worldgen/biome", "registry/worldgen/biome"},
        {"minecraft:zombie_nautilus_variant", "registry/zombie_nautilus_variant"}
    ].

%% @doc Load all 11 required registries from the registry/ directory.
-spec load_all_registries() -> [{string(), list()}].
load_all_registries() ->
    [ {RegId, load_registry(RegId, DirPath)} || {RegId, DirPath} <- required_registries() ].

%% @doc Load a single registry from a directory containing JSON files.
-spec load_registry(string(), string()) -> list().
load_registry(_RegId, DirPath) ->
    case file:list_dir(DirPath) of
        {ok, Files} ->
            JsonFiles = lists:sort([F || F <- Files, filename:extension(F) =:= ".json"]),
            lists:map(fun(File) ->
                FilePath = filename:join(DirPath, File),
                {ok, Bin} = file:read_file(FilePath),
                JsonMap = json:decode(Bin),
                NbtTag = json_to_nbt:json_to_nbt(JsonMap),
                EntryName = filename:rootname(File),
                EntryId = "minecraft:" ++ EntryName,
                [EntryId, {some, NbtTag}]
            end, JsonFiles);
        {error, _Reason} ->
            []
    end.

%% @doc Helper to get entry names (in order) for a registry.
-spec get_registry_entry_names(string()) -> [string()].
get_registry_entry_names(RegId) ->
    case lists:keyfind(RegId, 1, required_registries()) of
        {RegId, DirPath} ->
            case file:list_dir(DirPath) of
                {ok, Files} ->
                    JsonFiles = lists:sort([F || F <- Files, filename:extension(F) =:= ".json"]),
                    ["minecraft:" ++ filename:rootname(F) || F <- JsonFiles];
                {error, _} ->
                    []
            end;
        false ->
            []
    end.

%% @doc Helper to send all registry_data packets to a player state.
-spec send_all_registries(term()) -> ok.
send_all_registries(State) ->
    Registries = load_all_registries(),
    lists:foreach(fun({RegId, Entries}) ->
        Record = #'minecraft:registry_data'{registry_id = RegId, entries = Entries},
        player:send_message('minecraft:registry_data', Record, State)
    end, Registries),

    ok.
