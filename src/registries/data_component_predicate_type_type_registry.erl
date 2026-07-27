-module(data_component_predicate_type_type_registry).
-export([name_to_id/1, id_to_name/1]).

name_to_id('minecraft:damage') -> 0;
name_to_id('minecraft:enchantments') -> 1;
name_to_id('minecraft:stored_enchantments') -> 2;
name_to_id('minecraft:potion_contents') -> 3;
name_to_id('minecraft:custom_data') -> 4;
name_to_id('minecraft:container') -> 5;
name_to_id('minecraft:bundle_contents') -> 6;
name_to_id('minecraft:firework_explosion') -> 7;
name_to_id('minecraft:fireworks') -> 8;
name_to_id('minecraft:writable_book_content') -> 9;
name_to_id('minecraft:written_book_content') -> 10;
name_to_id('minecraft:attribute_modifiers') -> 11;
name_to_id('minecraft:trim') -> 12;
name_to_id('minecraft:jukebox_playable') -> 13;
name_to_id('minecraft:villager/variant') -> 14;
name_to_id(_) -> error(unknown_name).

id_to_name(0) -> 'minecraft:damage';
id_to_name(1) -> 'minecraft:enchantments';
id_to_name(2) -> 'minecraft:stored_enchantments';
id_to_name(3) -> 'minecraft:potion_contents';
id_to_name(4) -> 'minecraft:custom_data';
id_to_name(5) -> 'minecraft:container';
id_to_name(6) -> 'minecraft:bundle_contents';
id_to_name(7) -> 'minecraft:firework_explosion';
id_to_name(8) -> 'minecraft:fireworks';
id_to_name(9) -> 'minecraft:writable_book_content';
id_to_name(10) -> 'minecraft:written_book_content';
id_to_name(11) -> 'minecraft:attribute_modifiers';
id_to_name(12) -> 'minecraft:trim';
id_to_name(13) -> 'minecraft:jukebox_playable';
id_to_name(14) -> 'minecraft:villager/variant';
id_to_name(_) -> error(unknown_id).
