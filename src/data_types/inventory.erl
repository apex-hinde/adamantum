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
    set_slot/3,
    slot_count/1,
    to_slot_list/1,
    is_empty/1,
    same_item/2,
    max_stack/1,
    with_count/2,
    merge_slots/2,
    matches_hashed/2,
    container_click/6
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
%% Protocol slot lists — ordered by window index for Set Container Content
%% ---------------------------------------------------------------------------

%% Player inventory window: slots 0–45 (46 total).
slot_count(#player_inventory{}) -> 46;
slot_count(#grid_inventory{slots = A}) -> array:size(A);
slot_count(#dispenser_inventory{}) -> 9;
slot_count(#crafter_inventory{}) -> 9;
slot_count(#furnace_inventory{}) -> 3;
slot_count(#brewing_stand_inventory{}) -> 5;
slot_count(#hopper_inventory{}) -> 5;
slot_count(#crafting_inventory{}) -> 10;
slot_count(#anvil_inventory{}) -> 3;
slot_count(#beacon_inventory{}) -> 1;
slot_count(#enchantment_inventory{}) -> 2;
slot_count(#grindstone_inventory{}) -> 3;
slot_count(#lectern_inventory{}) -> 1;
slot_count(#loom_inventory{}) -> 4;
slot_count(#merchant_inventory{}) -> 3;
slot_count(#smithing_inventory{}) -> 4;
slot_count(#cartography_inventory{}) -> 3;
slot_count(#stonecutter_inventory{}) -> 2;
slot_count(#horse_inventory{chest = undefined}) -> 2;
slot_count(#horse_inventory{chest = Chest}) -> 2 + array:size(Chest);
slot_count(#llama_inventory{chest = Chest}) -> 2 + array:size(Chest).

%% Return every protocol slot for this inventory in index order (0 .. N-1).
to_slot_list(#player_inventory{} = Inv) ->
    [get_slot(Inv, I) || I <- lists:seq(0, 45)];
to_slot_list(Inv) ->
    N = slot_count(Inv),
    [get_slot(Inv, I) || I <- lists:seq(0, N - 1)].

%% ---------------------------------------------------------------------------
%% Slot helpers
%% ---------------------------------------------------------------------------

is_empty(#slot{item_count = C}) when C =< 0 -> true;
is_empty(#slot{itemID = undefined}) -> true;
is_empty(#slot{}) -> false;
is_empty(#hashed_slot{item_count = C}) when C =< 0 -> true;
is_empty(#hashed_slot{itemID = undefined}) -> true;
is_empty(#hashed_slot{}) -> false;
is_empty(_) -> true.

same_item(A, B) ->
    (not is_empty(A)) andalso (not is_empty(B))
        andalso slot_item_id(A) =:= slot_item_id(B)
        andalso slot_components_equal(A, B).

max_stack(#slot{components_to_add = Add}) ->
    max_stack_from_components(Add, 64);
max_stack(_) ->
    64.

with_count(_Slot, Count) when Count =< 0 ->
    empty_slot();
with_count(#slot{} = Slot, Count) ->
    Slot#slot{item_count = Count}.

%% Merge Source into Dest. Returns {NewDest, LeftoverSource}.
merge_slots(Source, Dest) ->
    case is_empty(Source) of
        true ->
            {Dest, empty_slot()};
        false ->
            case is_empty(Dest) of
                true ->
                    Max = max_stack(Source),
                    SrcN = Source#slot.item_count,
                    if
                        SrcN =< Max -> {with_count(Source, SrcN), empty_slot()};
                        true -> {with_count(Source, Max), with_count(Source, SrcN - Max)}
                    end;
                false ->
                    case same_item(Source, Dest) of
                        false ->
                            {Dest, Source};
                        true ->
                            Max = max_stack(Dest),
                            Space = Max - Dest#slot.item_count,
                            SrcN = Source#slot.item_count,
                            if
                                Space =< 0 -> {Dest, Source};
                                SrcN =< Space -> {with_count(Dest, Dest#slot.item_count + SrcN), empty_slot()};
                                true -> {with_count(Dest, Max), with_count(Source, SrcN - Space)}
                            end
                    end
            end
    end.

%% Compare server #slot{} to client #hashed_slot{} (item id + count).
matches_hashed(Slot, #hashed_slot{} = H) ->
    case {is_empty(Slot), is_empty(H)} of
        {true, true} -> true;
        {false, false} ->
            slot_item_id(Slot) =:= slot_item_id(H)
                andalso Slot#slot.item_count =:= H#hashed_slot.item_count;
        _ -> false
    end;
matches_hashed(Slot, _) ->
    is_empty(Slot).


%% container_click — serverbound click simulation (SPEC.md)
%% Returns {NewInv, NewCarried, TouchedSlots, NewDrag}
%% Drag is undefined | {Type, [Slot]} where Type is left | right | middle.

container_click(Mode, Button, Slot, Inv, Carried, Drag) ->
    Carried1 = case Carried of
        #slot{} -> Carried;
        _ -> empty_slot()
    end,
    %% Non-paint clicks reset painting status (SPEC).
    Drag1 = case Mode of
        5 -> Drag;
        _ -> undefined
    end,
    click(Mode, Button, Slot, Inv, Carried1, Drag1, []).

%% Mode 0 — left / right click
click(0, 0, -999, Inv, _Carried, _Drag, Touched) ->
    {Inv, empty_slot(), Touched, undefined};
click(0, 1, -999, Inv, Carried, _Drag, Touched) ->
    case is_empty(Carried) of
        true -> {Inv, Carried, Touched, undefined};
        false -> {Inv, with_count(Carried, Carried#slot.item_count - 1), Touched, undefined}
    end;
click(0, 0, Slot, Inv, Carried, Drag, Touched) when Slot >= 0 ->
    left_click(Slot, Inv, Carried, Drag, Touched);
click(0, 1, Slot, Inv, Carried, Drag, Touched) when Slot >= 0 ->
    right_click(Slot, Inv, Carried, Drag, Touched);

%% Mode 1 — shift click
click(1, _Button, Slot, Inv, Carried, Drag, Touched) when Slot >= 0 ->
    quick_move(Slot, Inv, Carried, Drag, Touched);

%% Mode 2 — number keys / offhand F
click(2, Button, Slot, Inv, Carried, Drag, Touched)
  when Slot >= 0, Button >= 0, Button =< 8 ->
    swap_slots(Slot, 36 + Button, Inv, Carried, Drag, Touched);
click(2, 40, Slot, Inv, Carried, Drag, Touched) when Slot >= 0 ->
    swap_slots(Slot, 45, Inv, Carried, Drag, Touched);

%% Mode 3 — middle click (creative only)
click(3, _Button, _Slot, Inv, Carried, _Drag, Touched) ->
    {Inv, Carried, Touched, undefined};

%% Mode 4 — drop key Q
click(4, 0, Slot, Inv, Carried, Drag, Touched) when Slot >= 0 ->
    drop_from_slot(Slot, 1, Inv, Carried, Drag, Touched);
click(4, 1, Slot, Inv, Carried, Drag, Touched) when Slot >= 0 ->
    drop_from_slot(Slot, all, Inv, Carried, Drag, Touched);

%% Mode 5 — painting / drag
click(5, 0, -999, Inv, Carried, _Drag, Touched) ->
    start_drag(left, Inv, Carried, Touched);
click(5, 4, -999, Inv, Carried, _Drag, Touched) ->
    start_drag(right, Inv, Carried, Touched);
click(5, 8, -999, Inv, Carried, _Drag, Touched) ->
    start_drag(middle, Inv, Carried, Touched);
click(5, 1, Slot, Inv, Carried, Drag, Touched) when Slot >= 0 ->
    add_drag(left, Slot, Inv, Carried, Drag, Touched);
click(5, 5, Slot, Inv, Carried, Drag, Touched) when Slot >= 0 ->
    add_drag(right, Slot, Inv, Carried, Drag, Touched);
click(5, 9, Slot, Inv, Carried, Drag, Touched) when Slot >= 0 ->
    add_drag(middle, Slot, Inv, Carried, Drag, Touched);
click(5, 2, -999, Inv, Carried, Drag, Touched) ->
    end_drag(left, Inv, Carried, Drag, Touched);
click(5, 6, -999, Inv, Carried, Drag, Touched) ->
    end_drag(right, Inv, Carried, Drag, Touched);
click(5, 10, -999, Inv, Carried, Drag, Touched) ->
    end_drag(middle, Inv, Carried, Drag, Touched);
click(5, _Button, _Slot, Inv, Carried, _Drag, Touched) ->
    {Inv, Carried, Touched, undefined};

%% Mode 6 — double click
click(6, _Button, Slot, Inv, Carried, Drag, Touched) when Slot >= 0 ->
    collect(Slot, Inv, Carried, Drag, Touched);

click(_Mode, _Button, _Slot, Inv, Carried, Drag, Touched) ->
    {Inv, Carried, Touched, Drag}.

%% --- mode 0 ---

left_click(Slot, Inv, Carried, Drag, Touched) ->
    Stack = safe_get(Inv, Slot),
    case is_empty(Carried) of
        true ->
            case is_empty(Stack) of
                true -> {Inv, Carried, Touched, Drag};
                false ->
                    {safe_set(Inv, Slot, empty_slot()), Stack, touch(Slot, Touched), Drag}
            end;
        false ->
            case is_empty(Stack) of
                true ->
                    {safe_set(Inv, Slot, Carried), empty_slot(), touch(Slot, Touched), Drag};
                false ->
                    case same_item(Carried, Stack) of
                        true ->
                            {NewStack, Left} = merge_slots(Carried, Stack),
                            %% no-op if dest already full
                            case Left#slot.item_count =:= Carried#slot.item_count
                                 andalso NewStack#slot.item_count =:= Stack#slot.item_count of
                                true -> {Inv, Carried, Touched, Drag};
                                false ->
                                    {safe_set(Inv, Slot, NewStack), Left, touch(Slot, Touched), Drag}
                            end;
                        false ->
                            {safe_set(Inv, Slot, Carried), Stack, touch(Slot, Touched), Drag}
                    end
            end
    end.

right_click(Slot, Inv, Carried, Drag, Touched) ->
    Stack = safe_get(Inv, Slot),
    case is_empty(Carried) of
        true ->
            case is_empty(Stack) of
                true -> {Inv, Carried, Touched, Drag};
                false ->
                    %% half to cursor; slot keeps the smaller half when odd (SPEC)
                    Take = (Stack#slot.item_count + 1) div 2,
                    Leave = Stack#slot.item_count - Take,
                    {safe_set(Inv, Slot, with_count(Stack, Leave)),
                     with_count(Stack, Take), touch(Slot, Touched), Drag}
            end;
        false ->
            case is_empty(Stack) of
                true ->
                    {safe_set(Inv, Slot, with_count(Carried, 1)),
                     with_count(Carried, Carried#slot.item_count - 1),
                     touch(Slot, Touched), Drag};
                false ->
                    case same_item(Carried, Stack) of
                        true ->
                            Max = max_stack(Stack),
                            case Stack#slot.item_count < Max of
                                true ->
                                    {safe_set(Inv, Slot, with_count(Stack, Stack#slot.item_count + 1)),
                                     with_count(Carried, Carried#slot.item_count - 1),
                                     touch(Slot, Touched), Drag};
                                false ->
                                    {Inv, Carried, Touched, Drag}
                            end;
                        false ->
                            {safe_set(Inv, Slot, Carried), Stack, touch(Slot, Touched), Drag}
                    end
            end
    end.


quick_move(Slot, Inv, Carried, Drag, Touched) ->
    Stack = safe_get(Inv, Slot),
    case is_empty(Stack) of
        true ->
            {Inv, Carried, Touched, Drag};
        false ->
            Targets = quick_move_targets(Slot),
            {Inv1, Left, Touched1} = place_into(Stack, Targets, Inv, Touched),
            case Left#slot.item_count =:= Stack#slot.item_count of
                true -> {Inv1, Carried, Touched1, Drag};
                false -> {safe_set(Inv1, Slot, Left), Carried, touch(Slot, Touched1), Drag}
            end
    end.

quick_move_targets(Slot) when Slot >= 36, Slot =< 44 -> lists:seq(9, 35);
quick_move_targets(Slot) when Slot >= 9, Slot =< 35 -> lists:seq(36, 44);
quick_move_targets(_) -> lists:seq(9, 44).

place_into(Stack, Targets, Inv, Touched) ->
    {Inv1, Stack1, Touched1} = place_pass(Stack, Targets, merge, Inv, Touched),
    case is_empty(Stack1) of
        true -> {Inv1, Stack1, Touched1};
        false -> place_pass(Stack1, Targets, empty, Inv1, Touched1)
    end.

place_pass(Stack, [], _Kind, Inv, Touched) ->
    {Inv, Stack, Touched};
place_pass(Stack, _Targets, _Kind, Inv, Touched) when Stack#slot.item_count =< 0 ->
    {Inv, empty_slot(), Touched};
place_pass(Stack, [T | Rest], Kind, Inv, Touched) ->
    Dest = safe_get(Inv, T),
    case Kind of
        merge ->
            case (not is_empty(Dest)) andalso same_item(Stack, Dest) of
                true ->
                    {NewDest, Left} = merge_slots(Stack, Dest),
                    place_pass(Left, Rest, Kind, safe_set(Inv, T, NewDest), touch(T, Touched));
                false ->
                    place_pass(Stack, Rest, Kind, Inv, Touched)
            end;
        empty ->
            case is_empty(Dest) of
                true ->
                    Max = max_stack(Stack),
                    N = Stack#slot.item_count,
                    if
                        N =< Max ->
                            {safe_set(Inv, T, Stack), empty_slot(), touch(T, Touched)};
                        true ->
                            place_pass(with_count(Stack, N - Max), Rest, Kind,
                                       safe_set(Inv, T, with_count(Stack, Max)),
                                       touch(T, Touched))
                    end;
                false ->
                    place_pass(Stack, Rest, Kind, Inv, Touched)
            end
    end.


swap_slots(A, A, Inv, Carried, Drag, Touched) ->
    {Inv, Carried, Touched, Drag};
swap_slots(A, B, Inv, Carried, Drag, Touched) ->
    SA = safe_get(Inv, A),
    SB = safe_get(Inv, B),
    Inv1 = safe_set(safe_set(Inv, A, SB), B, SA),
    {Inv1, Carried, touch(B, touch(A, Touched)), Drag}.


drop_from_slot(Slot, 1, Inv, Carried, Drag, Touched) ->
    Stack = safe_get(Inv, Slot),
    case is_empty(Stack) of
        true -> {Inv, Carried, Touched, Drag};
        false ->
            {safe_set(Inv, Slot, with_count(Stack, Stack#slot.item_count - 1)),
             Carried, touch(Slot, Touched), Drag}
    end;
drop_from_slot(Slot, all, Inv, Carried, Drag, Touched) ->
    Stack = safe_get(Inv, Slot),
    case is_empty(Stack) of
        true -> {Inv, Carried, Touched, Drag};
        false ->
            {safe_set(Inv, Slot, empty_slot()), Carried, touch(Slot, Touched), Drag}
    end.

%% --- mode 5 ---

start_drag(Type, Inv, Carried, Touched) ->
    case is_empty(Carried) of
        true -> {Inv, Carried, Touched, undefined};
        false -> {Inv, Carried, Touched, {Type, []}}
    end.

add_drag(Type, Slot, Inv, Carried, {Type, Slots}, Touched) ->
    case lists:member(Slot, Slots) of
        true -> {Inv, Carried, Touched, {Type, Slots}};
        false -> {Inv, Carried, Touched, {Type, [Slot | Slots]}}
    end;
add_drag(_Type, _Slot, Inv, Carried, _Drag, Touched) ->
    %% out of order painting packet — reset (SPEC)
    {Inv, Carried, Touched, undefined}.

end_drag(Type, Inv, Carried, {Type, Slots0}, Touched) ->
    Slots = lists:reverse(Slots0),
    {Inv1, Carried1, Touched1} = case Type of
        left -> paint_left(Slots, Inv, Carried, Touched);
        right -> paint_right(Slots, Inv, Carried, Touched);
        middle -> {Inv, Carried, Touched}  %% creative only
    end,
    {Inv1, Carried1, Touched1, undefined};
end_drag(_Type, Inv, Carried, _Drag, Touched) ->
    {Inv, Carried, Touched, undefined}.

paint_left([], Inv, Carried, Touched) ->
    {Inv, Carried, Touched};
paint_left(Slots, Inv, Carried, Touched) ->
    N = length(Slots),
    Each = Carried#slot.item_count div N,
    case Each < 1 of
        true -> {Inv, Carried, Touched};
        false ->
            lists:foldl(
                fun(Slot, {AccInv, AccCarried, AccTouched}) ->
                    case is_empty(AccCarried) of
                        true -> {AccInv, AccCarried, AccTouched};
                        false ->
                            Dest = safe_get(AccInv, Slot),
                            case is_empty(Dest) orelse same_item(AccCarried, Dest) of
                                false -> {AccInv, AccCarried, AccTouched};
                                true ->
                                    Place = with_count(AccCarried, Each),
                                    {NewDest, LeftPlace} = merge_slots(Place, Dest),
                                    Placed = Each - LeftPlace#slot.item_count,
                                    case Placed =< 0 of
                                        true -> {AccInv, AccCarried, AccTouched};
                                        false ->
                                            {safe_set(AccInv, Slot, NewDest),
                                             with_count(AccCarried, AccCarried#slot.item_count - Placed),
                                             touch(Slot, AccTouched)}
                                    end
                            end
                    end
                end,
                {Inv, Carried, Touched},
                Slots
            )
    end.

paint_right(Slots, Inv, Carried, Touched) ->
    lists:foldl(
        fun(Slot, {AccInv, AccCarried, AccTouched}) ->
            case is_empty(AccCarried) of
                true -> {AccInv, AccCarried, AccTouched};
                false ->
                    Dest = safe_get(AccInv, Slot),
                    Max = max_stack(AccCarried),
                    case is_empty(Dest) orelse (same_item(AccCarried, Dest) andalso Dest#slot.item_count < Max) of
                        false -> {AccInv, AccCarried, AccTouched};
                        true ->
                            NewDest = case is_empty(Dest) of
                                true -> with_count(AccCarried, 1);
                                false -> with_count(Dest, Dest#slot.item_count + 1)
                            end,
                            {safe_set(AccInv, Slot, NewDest),
                             with_count(AccCarried, AccCarried#slot.item_count - 1),
                             touch(Slot, AccTouched)}
                    end
            end
        end,
        {Inv, Carried, Touched},
        Slots
    ).

%% --- mode 6 ---

collect(Slot, Inv, Carried0, Drag, Touched0) ->
    {Inv1, Carried1, Touched1, _} =
        case is_empty(Carried0) of
            true -> left_click(Slot, Inv, Carried0, Drag, Touched0);
            false -> {Inv, Carried0, Touched0, Drag}
        end,
    case is_empty(Carried1) of
        true -> {Inv1, Carried1, Touched1, Drag};
        false ->
            Max = max_stack(Carried1),
            All = lists:seq(0, 45),
            Partials = [I || I <- All,
                             begin S = safe_get(Inv1, I),
                                   (not is_empty(S)) andalso same_item(S, Carried1)
                                       andalso S#slot.item_count < Max
                             end],
            Fulls = [I || I <- All,
                          begin S = safe_get(Inv1, I),
                                (not is_empty(S)) andalso same_item(S, Carried1)
                                    andalso S#slot.item_count >= Max
                          end],
            gather(Partials ++ Fulls, Inv1, Carried1, Touched1, Drag)
    end.

gather([], Inv, Carried, Touched, Drag) ->
    {Inv, Carried, Touched, Drag};
gather([I | Rest], Inv, Carried, Touched, Drag) ->
    Max = max_stack(Carried),
    case Carried#slot.item_count >= Max of
        true ->
            {Inv, Carried, Touched, Drag};
        false ->
            Stack = safe_get(Inv, I),
            case is_empty(Stack) orelse not same_item(Stack, Carried) of
                true ->
                    gather(Rest, Inv, Carried, Touched, Drag);
                false ->
                    {NewCarried, Left} = merge_slots(Stack, Carried),
                    gather(Rest, safe_set(Inv, I, Left), NewCarried, touch(I, Touched), Drag)
            end
    end.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

empty_array(Size, Default) ->
    array:new(Size, [{default, Default}, {fixed, true}]).

safe_get(Inv, Slot) ->
    try get_slot(Inv, Slot)
    catch error:{bad_slot, _, _} -> empty_slot();
          error:{bad_slot, _} -> empty_slot()
    end.

safe_set(Inv, Slot, Item) ->
    try set_slot(Inv, Slot, Item)
    catch error:{bad_slot, _, _} -> Inv;
          error:{bad_slot, _} -> Inv
    end.

touch(Slot, Touched) ->
    case lists:member(Slot, Touched) of
        true -> Touched;
        false -> [Slot | Touched]
    end.

slot_item_id(#slot{itemID = Id}) -> Id;
slot_item_id(#hashed_slot{itemID = Id}) -> Id.

%% Full component equality for stacking. Hashed slots only have type ids + hashes.
slot_components_equal(#slot{components_to_add = A1, components_to_remove = R1},
                      #slot{components_to_add = A2, components_to_remove = R2}) ->
    lists:sort(A1) =:= lists:sort(A2)
        andalso lists:sort(R1) =:= lists:sort(R2);
slot_components_equal(#slot{components_to_add = A1, components_to_remove = R1},
                      #hashed_slot{components_to_add = A2, components_to_remove = R2}) ->
    component_ids(A1) =:= component_ids(A2)
        andalso lists:sort(R1) =:= lists:sort(R2);
slot_components_equal(#hashed_slot{} = H, #slot{} = S) ->
    slot_components_equal(S, H);
slot_components_equal(#hashed_slot{components_to_add = A1, components_to_remove = R1},
                      #hashed_slot{components_to_add = A2, components_to_remove = R2}) ->
    lists:sort(A1) =:= lists:sort(A2)
        andalso lists:sort(R1) =:= lists:sort(R2);
slot_components_equal(_, _) ->
    false.

component_ids(List) when is_list(List) ->
    lists:sort([case C of {Id, _} -> Id; Id -> Id end || C <- List]);
component_ids(_) ->
    [].

max_stack_from_components([], Default) ->
    Default;
max_stack_from_components([{TypeId, Data} | Rest], Default) ->
    case TypeId of
        1 -> max_stack_value(Data, Default);
        'minecraft:max_stack_size' -> max_stack_value(Data, Default);
        max_stack_size -> max_stack_value(Data, Default);
        _ -> max_stack_from_components(Rest, Default)
    end;
max_stack_from_components([_ | Rest], Default) ->
    max_stack_from_components(Rest, Default).

max_stack_value(N, _Default) when is_integer(N), N > 0 -> N;
max_stack_value(#{max_stack_size := N}, _Default) when is_integer(N), N > 0 -> N;
max_stack_value({max_stack_size, N}, _Default) when is_integer(N), N > 0 -> N;
max_stack_value(_, Default) -> Default.
