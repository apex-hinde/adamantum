-module(villager_type_type_registry).
-export([name_to_id/1, id_to_name/1]).

name_to_id('minecraft:desert') -> 0;
name_to_id('minecraft:jungle') -> 1;
name_to_id('minecraft:plains') -> 2;
name_to_id('minecraft:savanna') -> 3;
name_to_id('minecraft:snow') -> 4;
name_to_id('minecraft:swamp') -> 5;
name_to_id('minecraft:taiga') -> 6;
name_to_id(_) -> error(unknown_name).

id_to_name(0) -> 'minecraft:desert';
id_to_name(1) -> 'minecraft:jungle';
id_to_name(2) -> 'minecraft:plains';
id_to_name(3) -> 'minecraft:savanna';
id_to_name(4) -> 'minecraft:snow';
id_to_name(5) -> 'minecraft:swamp';
id_to_name(6) -> 'minecraft:taiga';
id_to_name(_) -> error(unknown_id).
