-module(recipe_display_type_registry).
-export([name_to_id/1, id_to_name/1]).

name_to_id('minecraft:crafting_shapeless') -> 0;
name_to_id('minecraft:crafting_shaped') -> 1;
name_to_id('minecraft:furnace') -> 2;
name_to_id('minecraft:stonecutter') -> 3;
name_to_id('minecraft:smithing') -> 4;
name_to_id(_) -> error(unknown_name).

id_to_name(0) -> 'minecraft:crafting_shapeless';
id_to_name(1) -> 'minecraft:crafting_shaped';
id_to_name(2) -> 'minecraft:furnace';
id_to_name(3) -> 'minecraft:stonecutter';
id_to_name(4) -> 'minecraft:smithing';
id_to_name(_) -> error(unknown_id).
