-module(slot_display_type_registry).
-export([name_to_id/1, id_to_name/1]).

name_to_id('minecraft:empty') -> 0;
name_to_id('minecraft:any_fuel') -> 1;
name_to_id('minecraft:with_any_potion') -> 2;
name_to_id('minecraft:only_with_component') -> 3;
name_to_id('minecraft:item') -> 4;
name_to_id('minecraft:item_stack') -> 5;
name_to_id('minecraft:tag') -> 6;
name_to_id('minecraft:dyed') -> 7;
name_to_id('minecraft:smithing_trim') -> 8;
name_to_id('minecraft:with_remainder') -> 9;
name_to_id('minecraft:composite') -> 10;
name_to_id(_) -> error(unknown_name).

id_to_name(0) -> 'minecraft:empty';
id_to_name(1) -> 'minecraft:any_fuel';
id_to_name(2) -> 'minecraft:with_any_potion';
id_to_name(3) -> 'minecraft:only_with_component';
id_to_name(4) -> 'minecraft:item';
id_to_name(5) -> 'minecraft:item_stack';
id_to_name(6) -> 'minecraft:tag';
id_to_name(7) -> 'minecraft:dyed';
id_to_name(8) -> 'minecraft:smithing_trim';
id_to_name(9) -> 'minecraft:with_remainder';
id_to_name(10) -> 'minecraft:composite';
id_to_name(_) -> error(unknown_id).
