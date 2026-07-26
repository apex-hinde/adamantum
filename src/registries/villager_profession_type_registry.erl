-module(villager_profession_type_registry).
-export([name_to_id/1, id_to_name/1]).

name_to_id('minecraft:none') -> 0;
name_to_id('minecraft:armorer') -> 1;
name_to_id('minecraft:butcher') -> 2;
name_to_id('minecraft:cartographer') -> 3;
name_to_id('minecraft:cleric') -> 4;
name_to_id('minecraft:farmer') -> 5;
name_to_id('minecraft:fisherman') -> 6;
name_to_id('minecraft:fletcher') -> 7;
name_to_id('minecraft:leatherworker') -> 8;
name_to_id('minecraft:librarian') -> 9;
name_to_id('minecraft:mason') -> 10;
name_to_id('minecraft:nitwit') -> 11;
name_to_id('minecraft:shepherd') -> 12;
name_to_id('minecraft:toolsmith') -> 13;
name_to_id('minecraft:weaponsmith') -> 14;
name_to_id(_) -> error(unknown_name).

id_to_name(0) -> 'minecraft:none';
id_to_name(1) -> 'minecraft:armorer';
id_to_name(2) -> 'minecraft:butcher';
id_to_name(3) -> 'minecraft:cartographer';
id_to_name(4) -> 'minecraft:cleric';
id_to_name(5) -> 'minecraft:farmer';
id_to_name(6) -> 'minecraft:fisherman';
id_to_name(7) -> 'minecraft:fletcher';
id_to_name(8) -> 'minecraft:leatherworker';
id_to_name(9) -> 'minecraft:librarian';
id_to_name(10) -> 'minecraft:mason';
id_to_name(11) -> 'minecraft:nitwit';
id_to_name(12) -> 'minecraft:shepherd';
id_to_name(13) -> 'minecraft:toolsmith';
id_to_name(14) -> 'minecraft:weaponsmith';
id_to_name(_) -> error(unknown_id).
