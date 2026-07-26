-module(status).
-export([id_to_name/2, name_to_id/2]).

id_to_name(clientbound, 0) -> 'minecraft:status_response';
id_to_name(clientbound, 1) -> 'minecraft:pong_response';
id_to_name(serverbound, 0) -> 'minecraft:status_request';
id_to_name(serverbound, 1) -> 'minecraft:ping_request';
id_to_name(_, _) -> error(unknown_id).

name_to_id(clientbound, 'minecraft:status_response') -> 0;
name_to_id(clientbound, 'minecraft:pong_response') -> 1;
name_to_id(serverbound, 'minecraft:status_request') -> 0;
name_to_id(serverbound, 'minecraft:ping_request') -> 1;
name_to_id(_, _) -> error(unknown_name).
