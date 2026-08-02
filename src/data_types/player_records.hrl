-record(player_position, {
				x = 0 :: number(),
				y = 64 :: number(),
				z = 0 :: number(),
				yaw = 0 :: number(),
				pitch = 0 :: number(),
				on_ground = false :: boolean(),
				touching_wall = false :: boolean()
			   }).

-record(inventories, {
	player_invent,
	echest
}).