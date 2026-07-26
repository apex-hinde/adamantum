-module(handshake).
-export([id_to_name/2, name_to_id/2]).

id_to_name(serverbound, 0) -> 'minecraft:intention';
id_to_name(_, _) -> error(unknown_id).

name_to_id(serverbound, 'minecraft:intention') -> 0;
name_to_id(_, _) -> error(unknown_name).
