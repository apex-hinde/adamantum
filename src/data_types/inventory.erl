-module(inventory).

-include("src/data_types/inventory_records.hrl").

-export([
    empty_slot/0,
    new_player_inventory/0,
    new_grid_inventory/1,
    new_dispenser_inventory/0,
    new_crafter_inventory/0,
    new_furnace_inventory/0,
    new_brewing_stand_inventory/0,
    new_hopper_inventory/0,
    new_crafting_inventory/0,
    new_anvil_inventory/0,
    new_beacon_inventory/0,
    new_enchantment_inventory/0,
    new_grindstone_inventory/0,
    new_lectern_inventory/0,
    new_loom_inventory/0,
    new_merchant_inventory/0,
    new_smithing_inventory/0,
    new_cartography_inventory/0,
    new_stonecutter_inventory/0,
    new_horse_inventory/0,
    new_horse_inventory/1,
    new_llama_inventory/1,
    get_slot/2,
    set_slot/3
]).

%% Empty slot used to initialise inventories.
empty_slot() ->
    #slot{
        item_count = 0,
        itemID = undefined,
        components_to_add = [],
        components_to_remove = []
    }.

%% ---------------------------------------------------------------------------
%% Constructors — all slots empty
%% ---------------------------------------------------------------------------

new_player_inventory() ->
    Empty = empty_slot(),
    #player_inventory{
        crafting_output = Empty,
        crafting_input = empty_array(4, Empty),
        armor = empty_array(4, Empty),
        main = empty_array(27, Empty),
        hotbar = empty_array(9, Empty),
        offhand = Empty
    }.

%% Rows is 1..6 (generic_9xN, chest, barrel, ender chest, large chest, shulker).
new_grid_inventory(Rows) when is_integer(Rows), Rows >= 1, Rows =< 6 ->
    Empty = empty_slot(),
    #grid_inventory{
        rows = Rows,
        slots = empty_array(Rows * 9, Empty)
    }.

new_dispenser_inventory() ->
    #dispenser_inventory{slots = empty_array(9, empty_slot())}.

new_crafter_inventory() ->
    Empty = empty_slot(),
    #crafter_inventory{
        slots = empty_array(9, Empty),
        disabled = empty_array(9, false),
        result = Empty
    }.

new_furnace_inventory() ->
    Empty = empty_slot(),
    #furnace_inventory{
        ingredient = Empty,
        fuel = Empty,
        output = Empty
    }.

new_brewing_stand_inventory() ->
    Empty = empty_slot(),
    #brewing_stand_inventory{
        bottles = empty_array(3, Empty),
        ingredient = Empty,
        fuel = Empty
    }.

new_hopper_inventory() ->
    #hopper_inventory{slots = empty_array(5, empty_slot())}.

new_crafting_inventory() ->
    Empty = empty_slot(),
    #crafting_inventory{
        output = Empty,
        input = empty_array(9, Empty)
    }.

new_anvil_inventory() ->
    Empty = empty_slot(),
    #anvil_inventory{
        first = Empty,
        second = Empty,
        result = Empty
    }.

new_beacon_inventory() ->
    #beacon_inventory{payment = empty_slot()}.

new_enchantment_inventory() ->
    Empty = empty_slot(),
    #enchantment_inventory{
        item = Empty,
        lapis = Empty
    }.

new_grindstone_inventory() ->
    Empty = empty_slot(),
    #grindstone_inventory{
        first = Empty,
        second = Empty,
        result = Empty
    }.

new_lectern_inventory() ->
    #lectern_inventory{book = empty_slot()}.

new_loom_inventory() ->
    Empty = empty_slot(),
    #loom_inventory{
        banner = Empty,
        dye = Empty,
        pattern = Empty,
        result = Empty
    }.

new_merchant_inventory() ->
    Empty = empty_slot(),
    #merchant_inventory{
        input1 = Empty,
        input2 = Empty,
        result = Empty
    }.

new_smithing_inventory() ->
    Empty = empty_slot(),
    #smithing_inventory{
        template = Empty,
        base = Empty,
        additional = Empty,
        result = Empty
    }.

new_cartography_inventory() ->
    Empty = empty_slot(),
    #cartography_inventory{
        map = Empty,
        paper = Empty,
        output = Empty
    }.

new_stonecutter_inventory() ->
    Empty = empty_slot(),
    #stonecutter_inventory{
        input = Empty,
        result = Empty
    }.

%% Unchested horse / camel / skeleton horse / zombie horse.
new_horse_inventory() ->
    new_horse_inventory(false).

%% Chested = true for donkey/mule with chest (15 slots).
new_horse_inventory(false) ->
    Empty = empty_slot(),
    #horse_inventory{
        saddle = Empty,
        armor = Empty,
        chest = undefined
    };
new_horse_inventory(true) ->
    Empty = empty_slot(),
    #horse_inventory{
        saddle = Empty,
        armor = Empty,
        chest = empty_array(15, Empty)
    }.

%% Strength is 1..5; chest size = 3 * strength.
new_llama_inventory(Strength) when is_integer(Strength), Strength >= 1, Strength =< 5 ->
    Empty = empty_slot(),
    #llama_inventory{
        saddle = Empty,
        carpet = Empty,
        strength = Strength,
        chest = empty_array(3 * Strength, Empty)
    }.

%% ---------------------------------------------------------------------------
%% get_slot / set_slot — protocol window indices for this inventory's own slots
%% (player main/hotbar ranges that appear after container slots live in
%% #player_inventory{} and are addressed there, not on the container record).
%% ---------------------------------------------------------------------------

%% Player inventory (default window, never opened via Open Screen):
%% 0 crafting output, 1–4 crafting input, 5–8 armor, 9–35 main, 36–44 hotbar, 45 offhand.
get_slot(#player_inventory{crafting_output = S}, 0) ->
    S;
get_slot(#player_inventory{crafting_input = A}, I) when I >= 1, I =< 4 ->
    array:get(I - 1, A);
get_slot(#player_inventory{armor = A}, I) when I >= 5, I =< 8 ->
    array:get(I - 5, A);
get_slot(#player_inventory{main = A}, I) when I >= 9, I =< 35 ->
    array:get(I - 9, A);
get_slot(#player_inventory{hotbar = A}, I) when I >= 36, I =< 44 ->
    array:get(I - 36, A);
get_slot(#player_inventory{offhand = S}, 45) ->
    S;

%% Grid (chest, barrel, ender chest, shulker, generic_9xN): 0 .. rows*9-1.
get_slot(#grid_inventory{slots = A}, I) when is_integer(I), I >= 0 ->
    case I < array:size(A) of
        true -> array:get(I, A);
        false -> error({bad_slot, I})
    end;

%% Dispenser / dropper: 0–8.
get_slot(#dispenser_inventory{slots = A}, I) when I >= 0, I =< 8 ->
    array:get(I, A);

%% Crafter: 0–8 input, 45 result (protocol places result after player inv).
get_slot(#crafter_inventory{slots = A}, I) when I >= 0, I =< 8 ->
    array:get(I, A);
get_slot(#crafter_inventory{result = S}, 45) ->
    S;

%% Furnace / blast furnace / smoker: 0 ingredient, 1 fuel, 2 output.
get_slot(#furnace_inventory{ingredient = S}, 0) -> S;
get_slot(#furnace_inventory{fuel = S}, 1) -> S;
get_slot(#furnace_inventory{output = S}, 2) -> S;

%% Brewing stand: 0–2 bottles, 3 ingredient, 4 fuel.
get_slot(#brewing_stand_inventory{bottles = A}, I) when I >= 0, I =< 2 ->
    array:get(I, A);
get_slot(#brewing_stand_inventory{ingredient = S}, 3) -> S;
get_slot(#brewing_stand_inventory{fuel = S}, 4) -> S;

%% Hopper: 0–4.
get_slot(#hopper_inventory{slots = A}, I) when I >= 0, I =< 4 ->
    array:get(I, A);

%% Crafting table: 0 output, 1–9 input.
get_slot(#crafting_inventory{output = S}, 0) -> S;
get_slot(#crafting_inventory{input = A}, I) when I >= 1, I =< 9 ->
    array:get(I - 1, A);

%% Anvil: 0 first, 1 second, 2 result.
get_slot(#anvil_inventory{first = S}, 0) -> S;
get_slot(#anvil_inventory{second = S}, 1) -> S;
get_slot(#anvil_inventory{result = S}, 2) -> S;

%% Beacon: 0 payment.
get_slot(#beacon_inventory{payment = S}, 0) -> S;

%% Enchantment: 0 item, 1 lapis.
get_slot(#enchantment_inventory{item = S}, 0) -> S;
get_slot(#enchantment_inventory{lapis = S}, 1) -> S;

%% Grindstone: 0 first, 1 second, 2 result.
get_slot(#grindstone_inventory{first = S}, 0) -> S;
get_slot(#grindstone_inventory{second = S}, 1) -> S;
get_slot(#grindstone_inventory{result = S}, 2) -> S;

%% Lectern: 0 book.
get_slot(#lectern_inventory{book = S}, 0) -> S;

%% Loom: 0 banner, 1 dye, 2 pattern, 3 result.
get_slot(#loom_inventory{banner = S}, 0) -> S;
get_slot(#loom_inventory{dye = S}, 1) -> S;
get_slot(#loom_inventory{pattern = S}, 2) -> S;
get_slot(#loom_inventory{result = S}, 3) -> S;

%% Merchant: 0–1 input, 2 result.
get_slot(#merchant_inventory{input1 = S}, 0) -> S;
get_slot(#merchant_inventory{input2 = S}, 1) -> S;
get_slot(#merchant_inventory{result = S}, 2) -> S;

%% Smithing: 0 template, 1 base, 2 additional, 3 result.
get_slot(#smithing_inventory{template = S}, 0) -> S;
get_slot(#smithing_inventory{base = S}, 1) -> S;
get_slot(#smithing_inventory{additional = S}, 2) -> S;
get_slot(#smithing_inventory{result = S}, 3) -> S;

%% Cartography: 0 map, 1 paper, 2 output.
get_slot(#cartography_inventory{map = S}, 0) -> S;
get_slot(#cartography_inventory{paper = S}, 1) -> S;
get_slot(#cartography_inventory{output = S}, 2) -> S;

%% Stonecutter: 0 input, 1 result.
get_slot(#stonecutter_inventory{input = S}, 0) -> S;
get_slot(#stonecutter_inventory{result = S}, 1) -> S;

%% Horse: 0 saddle, 1 armor, 2–16 chest when present.
get_slot(#horse_inventory{saddle = S}, 0) -> S;
get_slot(#horse_inventory{armor = S}, 1) -> S;
get_slot(#horse_inventory{chest = Chest}, I)
  when is_integer(I), I >= 2, Chest =/= undefined ->
    Idx = I - 2,
    case Idx < array:size(Chest) of
        true -> array:get(Idx, Chest);
        false -> error({bad_slot, I})
    end;

%% Llama: 0 saddle, 1 carpet, 2 .. (1 + 3*strength) chest.
get_slot(#llama_inventory{saddle = S}, 0) -> S;
get_slot(#llama_inventory{carpet = S}, 1) -> S;
get_slot(#llama_inventory{chest = Chest}, I) when is_integer(I), I >= 2 ->
    Idx = I - 2,
    case Idx < array:size(Chest) of
        true -> array:get(Idx, Chest);
        false -> error({bad_slot, I})
    end;

get_slot(Inv, I) ->
    error({bad_slot, Inv, I}).

%% --- set_slot ---

set_slot(#player_inventory{} = Inv, 0, Slot) ->
    Inv#player_inventory{crafting_output = Slot};
set_slot(#player_inventory{crafting_input = A} = Inv, I, Slot) when I >= 1, I =< 4 ->
    Inv#player_inventory{crafting_input = array:set(I - 1, Slot, A)};
set_slot(#player_inventory{armor = A} = Inv, I, Slot) when I >= 5, I =< 8 ->
    Inv#player_inventory{armor = array:set(I - 5, Slot, A)};
set_slot(#player_inventory{main = A} = Inv, I, Slot) when I >= 9, I =< 35 ->
    Inv#player_inventory{main = array:set(I - 9, Slot, A)};
set_slot(#player_inventory{hotbar = A} = Inv, I, Slot) when I >= 36, I =< 44 ->
    Inv#player_inventory{hotbar = array:set(I - 36, Slot, A)};
set_slot(#player_inventory{} = Inv, 45, Slot) ->
    Inv#player_inventory{offhand = Slot};

set_slot(#grid_inventory{slots = A} = Inv, I, Slot) when is_integer(I), I >= 0 ->
    case I < array:size(A) of
        true -> Inv#grid_inventory{slots = array:set(I, Slot, A)};
        false -> error({bad_slot, I})
    end;

set_slot(#dispenser_inventory{slots = A} = Inv, I, Slot) when I >= 0, I =< 8 ->
    Inv#dispenser_inventory{slots = array:set(I, Slot, A)};

set_slot(#crafter_inventory{slots = A} = Inv, I, Slot) when I >= 0, I =< 8 ->
    Inv#crafter_inventory{slots = array:set(I, Slot, A)};
set_slot(#crafter_inventory{} = Inv, 45, Slot) ->
    Inv#crafter_inventory{result = Slot};

set_slot(#furnace_inventory{} = Inv, 0, Slot) ->
    Inv#furnace_inventory{ingredient = Slot};
set_slot(#furnace_inventory{} = Inv, 1, Slot) ->
    Inv#furnace_inventory{fuel = Slot};
set_slot(#furnace_inventory{} = Inv, 2, Slot) ->
    Inv#furnace_inventory{output = Slot};

set_slot(#brewing_stand_inventory{bottles = A} = Inv, I, Slot) when I >= 0, I =< 2 ->
    Inv#brewing_stand_inventory{bottles = array:set(I, Slot, A)};
set_slot(#brewing_stand_inventory{} = Inv, 3, Slot) ->
    Inv#brewing_stand_inventory{ingredient = Slot};
set_slot(#brewing_stand_inventory{} = Inv, 4, Slot) ->
    Inv#brewing_stand_inventory{fuel = Slot};

set_slot(#hopper_inventory{slots = A} = Inv, I, Slot) when I >= 0, I =< 4 ->
    Inv#hopper_inventory{slots = array:set(I, Slot, A)};

set_slot(#crafting_inventory{} = Inv, 0, Slot) ->
    Inv#crafting_inventory{output = Slot};
set_slot(#crafting_inventory{input = A} = Inv, I, Slot) when I >= 1, I =< 9 ->
    Inv#crafting_inventory{input = array:set(I - 1, Slot, A)};

set_slot(#anvil_inventory{} = Inv, 0, Slot) ->
    Inv#anvil_inventory{first = Slot};
set_slot(#anvil_inventory{} = Inv, 1, Slot) ->
    Inv#anvil_inventory{second = Slot};
set_slot(#anvil_inventory{} = Inv, 2, Slot) ->
    Inv#anvil_inventory{result = Slot};

set_slot(#beacon_inventory{} = Inv, 0, Slot) ->
    Inv#beacon_inventory{payment = Slot};

set_slot(#enchantment_inventory{} = Inv, 0, Slot) ->
    Inv#enchantment_inventory{item = Slot};
set_slot(#enchantment_inventory{} = Inv, 1, Slot) ->
    Inv#enchantment_inventory{lapis = Slot};

set_slot(#grindstone_inventory{} = Inv, 0, Slot) ->
    Inv#grindstone_inventory{first = Slot};
set_slot(#grindstone_inventory{} = Inv, 1, Slot) ->
    Inv#grindstone_inventory{second = Slot};
set_slot(#grindstone_inventory{} = Inv, 2, Slot) ->
    Inv#grindstone_inventory{result = Slot};

set_slot(#lectern_inventory{} = Inv, 0, Slot) ->
    Inv#lectern_inventory{book = Slot};

set_slot(#loom_inventory{} = Inv, 0, Slot) ->
    Inv#loom_inventory{banner = Slot};
set_slot(#loom_inventory{} = Inv, 1, Slot) ->
    Inv#loom_inventory{dye = Slot};
set_slot(#loom_inventory{} = Inv, 2, Slot) ->
    Inv#loom_inventory{pattern = Slot};
set_slot(#loom_inventory{} = Inv, 3, Slot) ->
    Inv#loom_inventory{result = Slot};

set_slot(#merchant_inventory{} = Inv, 0, Slot) ->
    Inv#merchant_inventory{input1 = Slot};
set_slot(#merchant_inventory{} = Inv, 1, Slot) ->
    Inv#merchant_inventory{input2 = Slot};
set_slot(#merchant_inventory{} = Inv, 2, Slot) ->
    Inv#merchant_inventory{result = Slot};

set_slot(#smithing_inventory{} = Inv, 0, Slot) ->
    Inv#smithing_inventory{template = Slot};
set_slot(#smithing_inventory{} = Inv, 1, Slot) ->
    Inv#smithing_inventory{base = Slot};
set_slot(#smithing_inventory{} = Inv, 2, Slot) ->
    Inv#smithing_inventory{additional = Slot};
set_slot(#smithing_inventory{} = Inv, 3, Slot) ->
    Inv#smithing_inventory{result = Slot};

set_slot(#cartography_inventory{} = Inv, 0, Slot) ->
    Inv#cartography_inventory{map = Slot};
set_slot(#cartography_inventory{} = Inv, 1, Slot) ->
    Inv#cartography_inventory{paper = Slot};
set_slot(#cartography_inventory{} = Inv, 2, Slot) ->
    Inv#cartography_inventory{output = Slot};

set_slot(#stonecutter_inventory{} = Inv, 0, Slot) ->
    Inv#stonecutter_inventory{input = Slot};
set_slot(#stonecutter_inventory{} = Inv, 1, Slot) ->
    Inv#stonecutter_inventory{result = Slot};

set_slot(#horse_inventory{} = Inv, 0, Slot) ->
    Inv#horse_inventory{saddle = Slot};
set_slot(#horse_inventory{} = Inv, 1, Slot) ->
    Inv#horse_inventory{armor = Slot};
set_slot(#horse_inventory{chest = Chest} = Inv, I, Slot)
  when is_integer(I), I >= 2, Chest =/= undefined ->
    Idx = I - 2,
    case Idx < array:size(Chest) of
        true -> Inv#horse_inventory{chest = array:set(Idx, Slot, Chest)};
        false -> error({bad_slot, I})
    end;

set_slot(#llama_inventory{} = Inv, 0, Slot) ->
    Inv#llama_inventory{saddle = Slot};
set_slot(#llama_inventory{} = Inv, 1, Slot) ->
    Inv#llama_inventory{carpet = Slot};
set_slot(#llama_inventory{chest = Chest} = Inv, I, Slot) when is_integer(I), I >= 2 ->
    Idx = I - 2,
    case Idx < array:size(Chest) of
        true -> Inv#llama_inventory{chest = array:set(Idx, Slot, Chest)};
        false -> error({bad_slot, I})
    end;

set_slot(Inv, I, _Slot) ->
    error({bad_slot, Inv, I}).

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

empty_array(Size, Default) ->
    array:new(Size, [{default, Default}, {fixed, true}]).
