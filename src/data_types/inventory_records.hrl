-include("src/data_types/records.hrl").
%% inventory records
-record(player_inventory, {
    crafting_output:: #slot{},       % ephemeral result
    crafting_input :: array:array(), % size 4 (2x2, index 1+x+2*y in window)
    armor          :: array:array(), % size 4 (head, chest, legs, feet)
    main           :: array:array(), % size 27
    hotbar         :: array:array(), % size 9
    offhand        :: #slot{}
}).

%% generic_9x1 .. generic_9x6, chest, barrel, ender chest, large chest, shulker_box.
%% rows*9 slots; large chest is two of these (or rows = 6).
-record(grid_inventory, {
    rows  :: 1..6,
    slots :: array:array()  % size = rows * 9
}).

%% dispenser, dropper — generic_3x3 contents (x + 3*y).
-record(dispenser_inventory, {
    slots :: array:array()  % size 9
}).

%% crafter_3x3 — 3x3 input, per-slot disabled flags, result.
-record(crafter_inventory, {
    slots    :: array:array(), % size 9 input
    disabled :: array:array(), % size 9 booleans
    result   :: #slot{}        % ephemeral / output
}).

%% furnace, blast_furnace, smoker.
-record(furnace_inventory, {
    ingredient :: #slot{},
    fuel       :: #slot{},
    output     :: #slot{}
}).

%% brewing_stand.
-record(brewing_stand_inventory, {
    bottles    :: array:array(), % size 3
    ingredient :: #slot{},
    fuel       :: #slot{}        % blaze powder
}).

%% hopper, minecart with hopper.
-record(hopper_inventory, {
    slots :: array:array()  % size 5
}).

%% crafting table (3x3); distinct from player 2x2 crafting.
-record(crafting_inventory, {
    output :: #slot{},
    input  :: array:array()  % size 9 (1+x+3*y in window after output)
}).

%% anvil.
-record(anvil_inventory, {
    first  :: #slot{},
    second :: #slot{},
    result :: #slot{}  % ephemeral
}).

%% beacon.
-record(beacon_inventory, {
    payment :: #slot{}
}).

%% enchantment table.
-record(enchantment_inventory, {
    item  :: #slot{},
    lapis :: #slot{}
}).

%% grindstone.
-record(grindstone_inventory, {
    first  :: #slot{},
    second :: #slot{},
    result :: #slot{}  % ephemeral
}).

%% lectern (player inventory not shown in this window).
-record(lectern_inventory, {
    book :: #slot{}
}).

%% loom.
-record(loom_inventory, {
    banner  :: #slot{},
    dye     :: #slot{},
    pattern :: #slot{},
    result  :: #slot{}  % ephemeral
}).

%% merchant (villager / wandering trader).
-record(merchant_inventory, {
    input1 :: #slot{},
    input2 :: #slot{},
    result :: #slot{}  % ephemeral
}).

%% smithing table.
-record(smithing_inventory, {
    template   :: #slot{},
    base       :: #slot{},
    additional :: #slot{},
    result     :: #slot{}  % ephemeral
}).

%% cartography table.
-record(cartography_inventory, {
    map    :: #slot{},
    paper  :: #slot{},
    output :: #slot{}  % ephemeral
}).

%% stonecutter.
-record(stonecutter_inventory, {
    input  :: #slot{},
    result :: #slot{}  % ephemeral
}).

%% horse, skeleton/zombie horse, camel — saddle + armor; no chest.
%% donkey / mule — same plus optional chest (up to 15 slots when chested).
-record(horse_inventory, {
    saddle :: #slot{},
    armor  :: #slot{},
    chest  :: undefined | array:array()  % undefined if unchested; size 15 when chested
}).

%% llama / trader llama — carpet + strength-scaled chest (3 * strength, max 15).
-record(llama_inventory, {
    saddle   :: #slot{},  % present in protocol layout but unused
    carpet   :: #slot{},
    strength :: 1..5,
    chest    :: array:array()  % size = 3 * strength
}).


