-module(consume_effect_type_type_registry).
-export([name_to_id/1, id_to_name/1]).

name_to_id('minecraft:apply_effects') -> 0;
name_to_id('minecraft:remove_effects') -> 1;
name_to_id('minecraft:clear_all_effects') -> 2;
name_to_id('minecraft:teleport_randomly') -> 3;
name_to_id('minecraft:play_sound') -> 4;
name_to_id(_) -> error(unknown_name).

id_to_name(0) -> 'minecraft:apply_effects';
id_to_name(1) -> 'minecraft:remove_effects';
id_to_name(2) -> 'minecraft:clear_all_effects';
id_to_name(3) -> 'minecraft:teleport_randomly';
id_to_name(4) -> 'minecraft:play_sound';
id_to_name(_) -> error(unknown_id).
