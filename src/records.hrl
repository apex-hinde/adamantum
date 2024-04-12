-record(player, 
        {
            username,
            uuid,
            eid
        }).

-record(entity,
        {
            eid,
            name,
            type

        }).
-record(chunk, 
        {
            type,
            metadata,
            block_light,
            sky_light
        }).
-record(chunk_column, 
        {
            full_column,
            chunks = [],
            biome
        }).
