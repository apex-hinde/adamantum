%% coords are {x,y,z,yaw,pitch}, dimention 0 = overworld, 1 = nether, 2 = end
-record(db_player, {username, uuid, eid, gamemode, coords, current_slot, dimension}).
-record(entity, {eid, name, type}).

-record(db_chunk, {block_type, block_light, sky_light, block_count}).
-record(db_chunk_column, {full_column, chunks=[], biome}).
