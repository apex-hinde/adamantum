-module(login).
-export([id_to_name/2, name_to_id/2]).

id_to_name(clientbound, 0) -> 'minecraft:login_disconnect';
id_to_name(clientbound, 1) -> 'minecraft:hello';
id_to_name(clientbound, 2) -> 'minecraft:login_finished';
id_to_name(clientbound, 3) -> 'minecraft:login_compression';
id_to_name(clientbound, 4) -> 'minecraft:custom_query';
id_to_name(clientbound, 5) -> 'minecraft:cookie_request';
id_to_name(serverbound, 0) -> 'minecraft:hello';
id_to_name(serverbound, 1) -> 'minecraft:key';
id_to_name(serverbound, 2) -> 'minecraft:custom_query_answer';
id_to_name(serverbound, 3) -> 'minecraft:login_acknowledged';
id_to_name(serverbound, 4) -> 'minecraft:cookie_response';
id_to_name(_, _) -> error(unknown_id).

name_to_id(clientbound, 'minecraft:login_disconnect') -> 0;
name_to_id(clientbound, 'minecraft:hello') -> 1;
name_to_id(clientbound, 'minecraft:login_finished') -> 2;
name_to_id(clientbound, 'minecraft:login_compression') -> 3;
name_to_id(clientbound, 'minecraft:custom_query') -> 4;
name_to_id(clientbound, 'minecraft:cookie_request') -> 5;
name_to_id(serverbound, 'minecraft:hello') -> 0;
name_to_id(serverbound, 'minecraft:key') -> 1;
name_to_id(serverbound, 'minecraft:custom_query_answer') -> 2;
name_to_id(serverbound, 'minecraft:login_acknowledged') -> 3;
name_to_id(serverbound, 'minecraft:cookie_response') -> 4;
name_to_id(_, _) -> error(unknown_name).
