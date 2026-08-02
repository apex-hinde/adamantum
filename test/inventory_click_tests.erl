-module(inventory_click_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/data_types/inventory_records.hrl").

stone() ->
    #slot{item_count = 10, itemID = 1, components_to_add = [], components_to_remove = []}.

dirt() ->
    #slot{item_count = 5, itemID = 2, components_to_add = [], components_to_remove = []}.

left_pickup_test() ->
    Inv0 = inventory:new_player_inventory(),
    Inv1 = inventory:set_slot(Inv0, 36, stone()),
    {Inv2, Carried, Touched, _} =
        inventory:container_click(0, 0, 36, Inv1, inventory:empty_slot(), undefined),
    ?assertEqual(true, inventory:is_empty(inventory:get_slot(Inv2, 36))),
    ?assertEqual(10, Carried#slot.item_count),
    ?assertEqual(1, Carried#slot.itemID),
    ?assertEqual([36], Touched).

left_place_test() ->
    Inv0 = inventory:new_player_inventory(),
    {Inv1, Carried1, _, _} =
        inventory:container_click(0, 0, 36, Inv0, stone(), undefined),
    Slot = inventory:get_slot(Inv1, 36),
    ?assertEqual(10, Slot#slot.item_count),
    ?assertEqual(true, inventory:is_empty(Carried1)).

right_half_test() ->
    Inv0 = inventory:set_slot(inventory:new_player_inventory(), 9, stone()),
    {Inv1, Carried, _, _} =
        inventory:container_click(0, 1, 9, Inv0, inventory:empty_slot(), undefined),
    Left = inventory:get_slot(Inv1, 9),
    ?assertEqual(5, Carried#slot.item_count),
    ?assertEqual(5, Left#slot.item_count).

right_half_odd_test() ->
    Odd = #slot{item_count = 7, itemID = 1, components_to_add = [], components_to_remove = []},
    Inv0 = inventory:set_slot(inventory:new_player_inventory(), 9, Odd),
    {Inv1, Carried, _, _} =
        inventory:container_click(0, 1, 9, Inv0, inventory:empty_slot(), undefined),
    Left = inventory:get_slot(Inv1, 9),
    %% take ceil half = 4, leave 3 (smaller) — SPEC
    ?assertEqual(4, Carried#slot.item_count),
    ?assertEqual(3, Left#slot.item_count).

swap_hotbar_test() ->
    Inv0 = inventory:set_slot(inventory:new_player_inventory(), 9, stone()),
    Inv1 = inventory:set_slot(Inv0, 36, dirt()),
    {Inv2, _, Touched, _} =
        inventory:container_click(2, 0, 9, Inv1, inventory:empty_slot(), undefined),
    S9 = inventory:get_slot(Inv2, 9),
    S36 = inventory:get_slot(Inv2, 36),
    ?assertEqual(2, S9#slot.itemID),
    ?assertEqual(1, S36#slot.itemID),
    ?assert(lists:member(9, Touched)),
    ?assert(lists:member(36, Touched)).

quick_move_hotbar_to_main_test() ->
    Inv0 = inventory:set_slot(inventory:new_player_inventory(), 36, stone()),
    {Inv1, _, _, _} =
        inventory:container_click(1, 0, 36, Inv0, inventory:empty_slot(), undefined),
    S9 = inventory:get_slot(Inv1, 9),
    ?assertEqual(true, inventory:is_empty(inventory:get_slot(Inv1, 36))),
    ?assertEqual(10, S9#slot.item_count).

drop_outside_test() ->
    {_Inv, Carried, _, _} =
        inventory:container_click(0, 0, -999, inventory:new_player_inventory(), stone(), undefined),
    ?assertEqual(true, inventory:is_empty(Carried)).

offhand_swap_test() ->
    Inv0 = inventory:set_slot(inventory:new_player_inventory(), 36, stone()),
    {Inv1, _, _, _} =
        inventory:container_click(2, 40, 36, Inv0, inventory:empty_slot(), undefined),
    Off = inventory:get_slot(Inv1, 45),
    ?assertEqual(true, inventory:is_empty(inventory:get_slot(Inv1, 36))),
    ?assertEqual(10, Off#slot.item_count).

merge_same_item_test() ->
    Inv0 = inventory:set_slot(inventory:new_player_inventory(), 9,
        #slot{item_count = 50, itemID = 1, components_to_add = [], components_to_remove = []}),
    Carried = #slot{item_count = 20, itemID = 1, components_to_add = [], components_to_remove = []},
    {Inv1, Left, _, _} = inventory:container_click(0, 0, 9, Inv0, Carried, undefined),
    S9 = inventory:get_slot(Inv1, 9),
    ?assertEqual(64, S9#slot.item_count),
    ?assertEqual(6, Left#slot.item_count).
