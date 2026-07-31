-module(msg_to_record).

-export([msg_to_record/1]).
-include("src/data_types/records.hrl").

msg_to_record(Msg) ->
    case Msg of
        {'minecraft:intention', [Protocol_version, Server_address, Server_port, Next_state]} ->
            Protocol_version2 = decode:extract_value(Protocol_version),
            Server_address2 = decode:extract_value(Server_address),
            Server_port2 = decode:extract_value(Server_port),
            Next_state2 = decode:extract_value(Next_state),
            #'minecraft:intention'{protocol_version = Protocol_version2, server_address = Server_address2, server_port = Server_port2, next_state = Next_state2};
        {'minecraft:ping_request', [Timestamp]} ->
            Timestamp2 = decode:extract_value(Timestamp),
            #'minecraft:pong_response'{timestamp = Timestamp2};
        {'minecraft:hello', [Name, UUID]} ->
            Name2 = decode:extract_value(Name),
            UUID2 = decode:extract_value(UUID),
            #'minecraft:hello_serverbound'{name = Name2, uuid = UUID2};
        {'minecraft:key', [Public_key, Nounce]} ->
            Public_key2 = decode:extract_value(Public_key),
            Nounce2 = decode:extract_value(Nounce),
            #'minecraft:key'{public_key = Public_key2, nonce = Nounce2};
        {'minecraft:login_acknowledged', []} ->
            #'minecraft:login_acknowledged'{};
        {'minecraft:cookie_response', [Cookie, Data]} ->
            Cookie2 = decode:extract_value(Cookie),
            Data2 = decode:extract_value(Data),
            #'minecraft:cookie_response'{cookie = Cookie2, data = Data2};
        {'minecraft:custom_payload', [Channel, Data]} ->
            Channel2 = decode:extract_value(Channel),
            Data2 = decode:extract_value(Data),
            #'minecraft:custom_payload'{channel = Channel2, data = Data2};
        {'minecraft:client_information', [Locale, View_distance, Chat_mode, Chat_colors, Displayed_skin_parts, Main_hand, Enable_text_filtering, Allow_server_listings, Particle_status]} ->
            Locale2 = decode:extract_value(Locale),
            View_distance2 = decode:extract_value(View_distance),
            Chat_mode2 = decode:extract_value(Chat_mode),
            Chat_colors2 = decode:extract_value(Chat_colors),
            Displayed_skin_parts2 = decode:extract_value(Displayed_skin_parts),
            Main_hand2 = decode:extract_value(Main_hand),
            Enable_text_filtering2 = decode:extract_value(Enable_text_filtering),
            Allow_server_listings2 = decode:extract_value(Allow_server_listings),
            Particle_status2 = decode:extract_value(Particle_status),
            #'minecraft:client_information'{
                locale = Locale2,
                view_distance = View_distance2,
                chat_mode = Chat_mode2,
                chat_colors = Chat_colors2,
                displayed_skin_parts = Displayed_skin_parts2,
                main_hand = Main_hand2,
                enable_text_filtering = Enable_text_filtering2,
                allow_server_listings = Allow_server_listings2,
                particle_status = Particle_status2
            };

        {'minecraft:select_known_packs', [KnownPacks]} ->
            KnownPacks2 = decode:extract_value(KnownPacks),
            #'minecraft:select_known_packs'{known_packs = KnownPacks2};
        {'minecraft:finish_configuration', []} ->
            #'minecraft:finish_configuration'{};
        {'minecraft:login', [EntityId, IsHardcore, DimensionNames, MaxPlayers, ViewDistance, SimulationDistance, ReducedDebugInfo, EnableRespawnScreen, DoLimitedCrafting, DimensionType, DimensionName, HashedSeed, GameMode, PreviousGameMode, IsDebug, IsFlat, DeathLocation, PortalCooldown, SeaLevel, OnlineMode, EnforcesSecureChat]} ->
            #'minecraft:login'{
                entity_id = decode:extract_value(EntityId),
                is_hardcore = decode:extract_value(IsHardcore),
                dimension_names = decode:extract_value(DimensionNames),
                max_players = decode:extract_value(MaxPlayers),
                view_distance = decode:extract_value(ViewDistance),
                simulation_distance = decode:extract_value(SimulationDistance),
                reduced_debug_info = decode:extract_value(ReducedDebugInfo),
                enable_respawn_screen = decode:extract_value(EnableRespawnScreen),
                do_limited_crafting = decode:extract_value(DoLimitedCrafting),
                dimension_type = decode:extract_value(DimensionType),
                dimension_name = decode:extract_value(DimensionName),
                hashed_seed = decode:extract_value(HashedSeed),
                game_mode = decode:extract_value(GameMode),
                previous_game_mode = decode:extract_value(PreviousGameMode),
                is_debug = decode:extract_value(IsDebug),
                is_flat = decode:extract_value(IsFlat),
                death_location = decode:extract_value(DeathLocation),
                portal_cooldown = decode:extract_value(PortalCooldown),
                sea_level = decode:extract_value(SeaLevel),
                online_mode = decode:extract_value(OnlineMode),
                enforces_secure_chat = decode:extract_value(EnforcesSecureChat)
            };
        {'minecraft:client_tick_end', []} ->
            #'minecraft:client_tick_end'{};
        {'minecraft:game_event', [Event, Value]} ->
            Event2 = decode:extract_value(Event),
            Value2 = decode:extract_value(Value),
            #'minecraft:game_event'{event = Event2, value = Value2};
        {'minecraft:accept_teleportation', [TeleportId]} ->
            TeleportId2 = decode:extract_value(TeleportId),
            #'minecraft:accept_teleportation'{teleport_id = TeleportId2};
        {'minecraft:move_player_pos', [X, Y, Z, Flags]} ->
            X2 = decode:extract_value(X),
            Y2 = decode:extract_value(Y),
            Z2 = decode:extract_value(Z),
            Flags2 = decode:extract_value(Flags),
            #'minecraft:move_player_pos'{
                x = X2,
                y = Y2,
                z = Z2,
                flags = Flags2
            };
        {'minecraft:move_player_pos_rot', [X, Y, Z, Yaw, Pitch, Flags]} ->
            X2 = decode:extract_value(X),
            Y2 = decode:extract_value(Y),
            Z2 = decode:extract_value(Z),
            Yaw2 = decode:extract_value(Yaw),
            Pitch2 = decode:extract_value(Pitch),
            Flags2 = decode:extract_value(Flags),
            #'minecraft:move_player_pos_rot'{
                x = X2,
                y = Y2,
                z = Z2,
                yaw = Yaw2,
                pitch = Pitch2,
                flags = Flags2
            };
        {'minecraft:move_player_rot', [Yaw, Pitch, Flags]} ->
            Yaw2 = decode:extract_value(Yaw),
            Pitch2 = decode:extract_value(Pitch),
            Flags2 = decode:extract_value(Flags),
            #'minecraft:move_player_rot'{
                yaw = Yaw2,
                pitch = Pitch2,
                flags = Flags2
            };
        {'minecraft:move_player_status_only', [Flags]} ->
            Flags2 = decode:extract_value(Flags),
            #'minecraft:move_player_status_only'{
                flags = Flags2
            };
        {'minecraft:set_chunk_cache_center', [ChunkX, ChunkZ]} ->
            ChunkX2 = decode:extract_value(ChunkX),
            ChunkZ2 = decode:extract_value(ChunkZ),
            #'minecraft:set_chunk_cache_center'{chunk_x = ChunkX2, chunk_z = ChunkZ2};
        {'minecraft:forget_level_chunk', [ChunkZ, ChunkX]} ->
            ChunkZ2 = decode:extract_value(ChunkZ),
            ChunkX2 = decode:extract_value(ChunkX),
            #'minecraft:forget_level_chunk'{chunk_z = ChunkZ2, chunk_x = ChunkX2};
        {'minecraft:player_loaded', []} ->
            #'minecraft:player_loaded'{};

        _ ->
            io:format("Error: Unknown message type: ~p~n", [Msg]),
            ok

    end.


