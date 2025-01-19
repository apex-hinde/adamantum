-record(db_mnesia_player, {username, uuid, eid, gamemode, position={0,0,0,0,0}, current_slot}).
-record(entity, {eid, name, type}).




-record(db_chunk, {type, metadata, block_light, sky_light}).
-record(db_chunk_column, {full_column, chunks=[],add_data=[], biome}).
