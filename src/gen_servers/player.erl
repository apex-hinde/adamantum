-module(player).

-include("src/data_types/records.hrl").
-include("src/data_types/chunk_records.hrl").
-include("src/data_types/player_records.hrl").


-behaviour(gen_server).
-export([start_link/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2, init/1, stop/1, send_message/3, send_message_after/3, send_message_after/4, get_name/0, encode_message/3, minecraft_client_information/2]).
-export([get_surrounding_chunk_managers/1, get_surrounding_chunk_managers/2, fetch_chunk_managers/1, query_chunk/3, query_block/4, modify_block/5]).

-define(SERVER, ?MODULE).
-define(HANDSHAKE, 0).
-define(STATUS, 1).
-define(LOGIN, 2).
-define(TRANSFER, 3).
-define(CONFIGURATION, 4).
-define(PLAY, 5).

-record(state, {
    listen_pid,
    listen_socket,
    player_socket,
    queue,
    state_of_play,
    uuid,
    keep_alive,
    teleport_id,
    chunk_managers = [],
    render_distance = 12,
    current_chunk = {0,0},
    player_data

}).

%% api
get_name() ->
    ?SERVER.
stop(Name) ->
    gen_server:call(Name, stop).

%% chunk manager calls
get_surrounding_chunk_managers(PlayerPid) when is_pid(PlayerPid) ->
    gen_server:call(PlayerPid, get_stored_chunk_managers).

get_surrounding_chunk_managers(X, Z) ->
    world_manager:get_chunk_managers(X, Z).

fetch_chunk_managers(PlayerPid) ->
    gen_server:call(PlayerPid, fetch_chunk_managers).

query_chunk(ChunkManagerPid, ChunkX, ChunkZ) ->
    chunk_manager:get_chunk(ChunkManagerPid, ChunkX, ChunkZ).

query_block(ChunkManagerPid, BlockX, BlockY, BlockZ) ->
    chunk_manager:get_block(ChunkManagerPid, BlockX, BlockY, BlockZ).

modify_block(ChunkManagerPid, BlockX, BlockY, BlockZ, BlockId) ->
    chunk_manager:set_block(ChunkManagerPid, BlockX, BlockY, BlockZ, BlockId).

send_chunk_to_player(ChunkX, ChunkZ, State) ->
    ChunkManagerPid = world_manager:get_chunk_manager(ChunkX, ChunkZ),
    case chunk_manager:get_chunk(ChunkManagerPid, ChunkX, ChunkZ) of
        {ok, ChunkColumn} ->
            Record = chunk:chunk_column_to_level_chunk_record(ChunkColumn),
            player:send_message('minecraft:level_chunk_with_light', Record, State);
        {error, _Reason} ->
            {ok, State}
    end.



start_link(Listen_pid, Listen_socket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Listen_pid, Listen_socket], []).

init([Listen_pid, Listen_socket]) ->
    self() ! run_accept,
    {ok, #state{listen_pid = Listen_pid,
            listen_socket = Listen_socket,
            queue = <<>>,
            state_of_play = ?HANDSHAKE,
            player_data = #player_position{}}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(get_stored_chunk_managers, _From, State) ->
    {reply, State#state.chunk_managers, State};

handle_call(fetch_chunk_managers, _From, State) ->
    BlockX = trunc(State#state.player_data#player_position.x),
    BlockZ = trunc(State#state.player_data#player_position.z),
    Managers = world_manager:get_chunk_managers(BlockX, BlockZ),
    NewState = State#state{chunk_managers = Managers},
    {reply, Managers, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

%% to connect the player
handle_info(run_accept, State) ->

    {ok, Socket} = gen_tcp:accept(State#state.listen_socket),
    gen_server:cast(State#state.listen_pid, connected),
    {noreply, State#state{player_socket = Socket}};

handle_info({tcp, _Socket, Data}, State) ->

    Queue = State#state.queue,
    Data2 =
        case Queue of <<>> -> Data; _ -> <<Queue/binary, Data/binary>> end,
    NewState = 
        case decode:decode_type(Data2, varint) of
            {_, error} ->
                State#state{queue = Data2};
                
            {_Data3, _Length} ->
                Next_state = State#state{queue = <<>>},
                message(Data2, Next_state)
        end,
    {noreply, NewState};

handle_info({send_message, PacketName, Record}, State) ->
    send_message(PacketName, Record, State),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _Socket, _Reason}, State) ->
    {stop, normal, State}.

message(Data, State) ->
    {Data2, Length2} = decode:decode_type(Data, varint),
    Length = decode:extract_value(Length2),

    Length_of_data = byte_size(Data2),
    NewState = 
        if Length =:= Length_of_data ->

            process_message(Data2, State);
        Length < Length_of_data ->
            <<Interim_data:Length/binary, Rest/binary>> = Data2,
            Next_state = process_message(Interim_data, State),
            message(Rest, Next_state);
        Length > Length_of_data ->
            State#state{queue = Data}
        end,
    NewState.


process_message(Data, State) ->

    {Data2, Packet_ID2} = decode:decode_type(Data, varint),
    Packet_ID = decode:extract_value(Packet_ID2),
    case State#state.state_of_play of        
        ?HANDSHAKE ->
            Packet_name = handshake:id_to_name(serverbound, Packet_ID),
            Decoded = decode:decode_message(Data2, Packet_name),
            io:format("handshake | serverbound: ~p~n", [Decoded]),
            handle_decoded_message({Packet_name, Decoded}, State);
            
        ?STATUS ->
            Packet_name = status:id_to_name(serverbound, Packet_ID),

            Decoded = decode:decode_message(Data2, Packet_name),
            io:format("status | serverbound: ~p~n", [Decoded]),
            handle_decoded_message({Packet_name, Decoded}, State);
            
        ?LOGIN ->
            Packet_name = login:id_to_name(serverbound, Packet_ID),

            Decoded = decode:decode_message(Data2, Packet_name),
            io:format("login | serverbound: ~p~n", [Decoded]),
            handle_decoded_message({Packet_name, Decoded}, State);
            
        ?CONFIGURATION ->
            Packet_name = configuration:id_to_name(serverbound, Packet_ID),
            Decoded = decode:decode_message(Data2, Packet_name),
            io:format("configuration | serverbound: ~p~n", [Decoded]),
            handle_decoded_message({Packet_name, Decoded}, State);
        ?PLAY ->
            Packet_name = play:id_to_name(serverbound, Packet_ID),
            io:format("~p : ", [Packet_name]),
            io:format("~p~n", [Data]),
            Decoded = decode:decode_message(Data2, Packet_name),
            handle_decoded_message({Packet_name, Decoded}, State)
    end.



handle_decoded_message(Data, State) ->
    %% login
    {ok, NewState} = decode_message(Data, State),

    NewState.

send_message(PacketName, Record, State) ->
%%    io:format("clientbound: ~p~n", [Record]),

    {Encoded, NewState} = encode_message(PacketName, Record, State),
    Message = encode:encode_message(Encoded, PacketName, State#state.state_of_play),
    Length = encode:encode_type(byte_size(Message), varint),
    gen_tcp:send(State#state.player_socket, <<Length/binary, Message/binary>>),
    {ok, NewState}.

send_message_after(Time, PacketName, Record) when is_integer(Time) ->
    erlang:send_after(Time, self(), {send_message, PacketName, Record});
send_message_after(PacketName, Record, _State) ->
    send_message_after(0, PacketName, Record).

send_message_after(Time, PacketName, Record, _State) when is_integer(Time) ->
    send_message_after(Time, PacketName, Record).









%% decode message
decode_message(Data, State) ->
    case Data of
        {'minecraft:intention', Record} ->
            minecraft_intetion(Record, State);
    %% status
        {'minecraft:status_request', _} ->
            minecraft_status_request(State);
        {'minecraft:ping_request', Record} ->
            minecraft_ping_request(Record, State);
        {'minecraft:hello', Record} ->
            minecraft_hello(Record, State);
%%        {'minecraft:key', Record} ->
%%            minecraft_key(Record, State);
        {'minecraft:login_acknowledged', Record} ->
           minecraft_login_acknowledged(Record, State);
%%        {'minecraft:cookie_response', Record} ->
%%            minecraft_cookie_response(Record, State);
        {'minecraft:custom_payload', Record} ->
           minecraft_custom_payload(Record, State);
        {'minecraft:client_information', Record} ->
           minecraft_client_information(Record, State);
        {'minecraft:select_known_packs', Record} ->
           minecraft_select_known_packs(Record, State);
        {'minecraft:finish_configuration', Record} ->
           minecraft_finish_configuration(Record, State);
        {'minecraft:client_tick_end', Record} ->
            minecraft_client_tick_end(Record, State);
        {'minecraft:accept_teleportation', Record} ->
            minecraft_accept_teleportation(Record, State);
        {'minecraft:move_player_pos', Record} ->
            minecraft_move_player_pos(Record, State);
        {'minecraft:move_player_pos_rot', Record} ->
            minecraft_move_player_pos_rot(Record, State);
        {'minecraft:move_player_rot', Record} ->
            minecraft_move_player_rot(Record, State);
        {'minecraft:move_player_status_only', Record} ->
            minecraft_move_player_status_only(Record, State);
        {'minecraft:player_loaded', _Record} ->
            {ok, State};
        _ ->
            io:format("Error: Unknown message type, decode: ~p~n", [Data]),
            {ok, State}
    end.

minecraft_intetion(Record, State) ->
    {ok, State#state{state_of_play = Record#'minecraft:intention'.next_state}}.

minecraft_status_request(State) ->
    {ok, Bin} = file:read_file("src/configuration/status_response.json"),
 
    Response = #'minecraft:status_response'{
        json_response = Bin
        },
    {ok, NewState} = player:send_message('minecraft:status_response', Response, State),
    {ok, NewState#state{state_of_play = ?LOGIN}}.

minecraft_ping_request(Record, State) ->
    Response = #'minecraft:pong_response'{timestamp = Record#'minecraft:ping_request'.timestamp},
    {ok, NewState} = player:send_message('minecraft:pong_response', Response, State),
    {ok, NewState}.
minecraft_hello(Record, State) ->
%%    case mojang_api:get_player_uuid(Record#'minecraft:hello_serverbound'.name) of
%%        error -> player:stop(player:get_name());
%%        UUID -> case UUID == Record#'minecraft:hello_serverbound'.uuid of
%%            true -> ok;
%%            false -> player:stop(player:get_name())
%%        end
%%    end,
%%    Mojang_profile = mojang_api:get_profile(UUID),
%%
%%    Profile = #game_profile{}
    Profile = #'game_profile'{
        uuid = Record#'minecraft:hello_serverbound'.uuid,
        username = Record#'minecraft:hello_serverbound'.name,
        properties = []
        },
    Response = #'minecraft:login_finished'{
        session_id = Record#'minecraft:hello_serverbound'.uuid,
        profile = Profile},
     {ok, NewState} = player:send_message('minecraft:login_finished', Response, State),

    {ok, NewState#state{uuid = Record#'minecraft:hello_serverbound'.uuid}}.
minecraft_login_acknowledged(_Record, State) ->
    NewState = State#state{state_of_play = ?CONFIGURATION},
    KnownPacks = [["minecraft", "core", "26.2"]],
    {ok, NewState2} = player:send_message('minecraft:select_known_packs', #'minecraft:select_known_packs'{known_packs = KnownPacks}, NewState),
    {ok, NewState2}.

minecraft_select_known_packs(_Record, State) ->
    registry_loader:send_all_registries(State),
    Tags = tag_loader:load_all_tags(),
    {ok, NewState2} = player:send_message('minecraft:update_tags', #'minecraft:update_tags'{tags = Tags}, State),
    {ok, NewState3} = player:send_message('minecraft:finish_configuration', #'minecraft:finish_configuration'{}, NewState2),
    {ok, NewState3}.

minecraft_custom_payload(_Record, State) ->
    {ok, State}.

minecraft_client_information(_Record, State) ->
    {ok, State}.

minecraft_finish_configuration(_Record, State) ->
    NewState = State#state{state_of_play = ?PLAY},
    LoginPacket = #'minecraft:login'{
        entity_id = 1,
        is_hardcore = false,
        dimension_names = ["minecraft:overworld"],
        max_players = 20,
        view_distance = 12,
        simulation_distance = 12,
        reduced_debug_info = true,
        enable_respawn_screen = true,
        do_limited_crafting = false,
        dimension_type = 0,
        dimension_name = "minecraft:overworld",
        hashed_seed = 0,
        game_mode = 1,
        previous_game_mode = -1,
        is_debug = false,
        is_flat = false,
        death_location = #prefixed_optional{some = none, prefixed_optional = none},
        portal_cooldown = 0,
        sea_level = 63,
        online_mode = false,
        enforces_secure_chat = false
    },
    {ok, NewState2} = player:send_message('minecraft:login', LoginPacket, NewState),
    GameEvent = #'minecraft:game_event'{event = 13, value = 0.0},
    {ok, NewState3} = player:send_message('minecraft:game_event', GameEvent, NewState2),
    PlayerPosition = #'minecraft:player_position'{
        teleport_id = 100,
        x = NewState3#state.player_data#player_position.x,
        y = NewState3#state.player_data#player_position.y,
        z = NewState3#state.player_data#player_position.z,
        velocity_x = 0,
        velocity_y = 0,
        velocity_z = 0,
        yaw = NewState3#state.player_data#player_position.yaw,
        pitch = NewState3#state.player_data#player_position.pitch,
        flags = 0
    },
    {ok, NewState4} = player:send_message('minecraft:player_position', PlayerPosition, NewState3),
    PlayerInfo = #'minecraft:player_info_update'{
        actions = 0,
        players = [NewState4#state.uuid]
    },
    {ok, NewState5} = player:send_message('minecraft:player_info_update', PlayerInfo, NewState4),
    {ok, NewState6} = player:send_message('minecraft:set_chunk_cache_center', #'minecraft:set_chunk_cache_center'{chunk_x = 0, chunk_z = 0}, NewState5),
    {ok, NewState7} = send_surrounding_chunks(0, 0, 12, NewState6),
    {ok, NewState7}.

minecraft_accept_teleportation(Record, State) ->
    _TeleportId = Record#'minecraft:accept_teleportation'.teleport_id,
    {ok, State}.
minecraft_move_player_pos(Record, State) ->
    CurrentPos = State#state.player_data,
    Flags = Record#'minecraft:move_player_pos'.flags,
    NewPos = CurrentPos#player_position{
        x = Record#'minecraft:move_player_pos'.x,
        y = Record#'minecraft:move_player_pos'.y,
        z = Record#'minecraft:move_player_pos'.z,
        on_ground = (Flags band 16#01) =/= 0,
        touching_wall = (Flags band 16#02) =/= 0
    },
    check_and_update_chunks(Record#'minecraft:move_player_pos'.x, Record#'minecraft:move_player_pos'.z, State),
    {ok, State#state{player_data = NewPos}}.

minecraft_move_player_pos_rot(Record, State) ->
    CurrentPos = State#state.player_data,
    Flags = Record#'minecraft:move_player_pos_rot'.flags,
    NewPos = CurrentPos#player_position{
        x = Record#'minecraft:move_player_pos_rot'.x,
        y = Record#'minecraft:move_player_pos_rot'.y,
        z = Record#'minecraft:move_player_pos_rot'.z,
        yaw = Record#'minecraft:move_player_pos_rot'.yaw,
        pitch = Record#'minecraft:move_player_pos_rot'.pitch,
        on_ground = (Flags band 16#01) =/= 0,
        touching_wall = (Flags band 16#02) =/= 0
    },
    check_and_update_chunks(Record#'minecraft:move_player_pos_rot'.x, Record#'minecraft:move_player_pos_rot'.z, State),
    {ok, State#state{player_data = NewPos}}.

minecraft_move_player_rot(Record, State) ->
    CurrentPos = State#state.player_data,
    Flags = Record#'minecraft:move_player_rot'.flags,
    NewPos = CurrentPos#player_position{
        yaw = Record#'minecraft:move_player_rot'.yaw,
        pitch = Record#'minecraft:move_player_rot'.pitch,
        on_ground = (Flags band 16#01) =/= 0,
        touching_wall = (Flags band 16#02) =/= 0
    },
    {ok, State#state{player_data = NewPos}}.

minecraft_move_player_status_only(Record, State) ->
    CurrentPos = State#state.player_data,
    Flags = Record#'minecraft:move_player_status_only'.flags,
    NewPos = CurrentPos#player_position{
        on_ground = (Flags band 16#01) =/= 0,
        touching_wall = (Flags band 16#02) =/= 0
    },
    
    {ok, State#state{player_data = NewPos}}.

pos_to_chunk_coord(Val) ->
    erlang:floor(Val / 16.0).

check_and_update_chunks(NewX, NewZ, State) ->
    OldChunk = State#state.current_chunk,
    NewChunk = {pos_to_chunk_coord(NewX), pos_to_chunk_coord(NewZ)},
    if
        NewChunk =/= OldChunk ->
            handle_chunk_change(OldChunk, NewChunk, State);
        true ->
            {ok, State}
    end.

handle_chunk_change(OldChunk, NewChunk, State) ->
    R = State#state.render_distance,
    {OldChunkX, OldChunkZ} = OldChunk,
    {NewChunkX, NewChunkZ} = NewChunk,
    SetCenter = #'minecraft:set_chunk_cache_center'{chunk_x = NewChunkX, chunk_z = NewChunkZ},
    {ok, State2} = player:send_message('minecraft:set_chunk_cache_center', SetCenter, State),
    OldSet = ordsets:from_list([{X, Z} || X <- lists:seq(OldChunkX - R, OldChunkX + R), Z <- lists:seq(OldChunkZ - R, OldChunkZ + R)]),
    NewSet = ordsets:from_list([{X, Z} || X <- lists:seq(NewChunkX - R, NewChunkX + R), Z <- lists:seq(NewChunkZ - R, NewChunkZ + R)]),
    ToUnload = ordsets:subtract(OldSet, NewSet),
    ToLoad   = ordsets:subtract(NewSet, OldSet),
    State3 = lists:foldl(
        fun({UnloadX, UnloadZ}, AccState) ->
            UnloadRecord = #'minecraft:forget_level_chunk'{chunk_x = UnloadX, chunk_z = UnloadZ},
            {ok, NextState} = player:send_message('minecraft:forget_level_chunk', UnloadRecord, AccState),
            NextState
        end,
        State2,
        ToUnload
    ),
    State4 = lists:foldl(
        fun({LoadX, LoadZ}, AccState) ->
            {ok, NewState} = send_chunk_to_player(LoadX, LoadZ, AccState),
            NewState
        end,
        State3,
        ToLoad
    ),
    {ok, State4}.



    
    

send_surrounding_chunks(CenterChunkX, CenterChunkZ, RenderDistance, State) ->
    ChunkCoords = [
        {X, Z} 
        || X <- lists:seq(CenterChunkX - RenderDistance, CenterChunkX + RenderDistance),
           Z <- lists:seq(CenterChunkZ - RenderDistance, CenterChunkZ + RenderDistance)
    ],
    FinalState = lists:foldl(
        fun({CX, CZ}, AccState) ->
            {ok, NewState} = send_chunk_to_player(CX, CZ, AccState),
            NewState
        end, 
        State, 
        ChunkCoords
    ),
    {ok, FinalState}.


minecraft_client_tick_end(_Record, State) ->
    {ok, State}.










%% encode messages

encode_message(PacketName, Record, State) ->
    case PacketName of
        'minecraft:status_response' ->
            encode_status_response(Record, State);
        'minecraft:pong_response' -> 
            encode_pong_request(Record, State);
        'minecraft:login_finished' -> 
            encode_login_finished(Record, State);
        'minecraft:registry_data' ->
            encode_registry_data(Record, State);
        'minecraft:finish_configuration' ->
            encode_finish_configuration(Record, State);
        'minecraft:select_known_packs' ->
            encode_select_known_packs(Record, State);
        'minecraft:update_tags' ->
            encode_update_tags(Record, State);
        'minecraft:login' ->
            encode_login(Record, State);
        'minecraft:game_event' ->
            encode_game_event(Record, State);
        'minecraft:player_info_update' ->
            encode_player_info_update(Record, State);
        'minecraft:player_position' ->
            encode_player_position(Record, State);
        'minecraft:synchronize_player_position' ->
            encode_player_position(Record, State);
        synchronize_player_position ->
            encode_player_position(Record, State);
        'minecraft:set_chunk_cache_center' ->
            encode_set_chunk_cache_center(Record, State);
        set_chunk_cache_center ->
            encode_set_chunk_cache_center(Record, State);
        'minecraft:forget_level_chunk' ->
            encode_forget_level_chunk(Record, State);
        forget_level_chunk ->
            encode_forget_level_chunk(Record, State);
        'minecraft:level_chunk_with_light' ->
            encode_level_chunk_with_light(Record, State)
    end.

encode_status_response(Record, State) ->
    {{'minecraft:status_response', [Record#'minecraft:status_response'.json_response]}, State}.

encode_pong_request(Record, State) ->
    {{'minecraft:pong_response', [Record#'minecraft:pong_response'.timestamp]}, State}.

encode_login_finished(Record, State) ->
    {{'minecraft:login_finished', [Record#'minecraft:login_finished'.profile, Record#'minecraft:login_finished'.session_id]}, State}.

encode_registry_data(#'minecraft:registry_data'{registry_id = RegId, entries = Entries}, State) ->
    {{'minecraft:registry_data', [RegId, Entries]}, State};
encode_registry_data({'minecraft:registry_data', [RegId, Entries]}, State) ->
    {{'minecraft:registry_data', [RegId, Entries]}, State}.

encode_finish_configuration(_Record, State) ->
    {{'minecraft:finish_configuration', []}, State}.

encode_select_known_packs(#'minecraft:select_known_packs'{known_packs = KnownPacks}, State) ->
    {{'minecraft:select_known_packs', [KnownPacks]}, State}.

encode_update_tags(#'minecraft:update_tags'{tags = Tags}, State) ->
    {{'minecraft:update_tags', [Tags]}, State}.

encode_login(#'minecraft:login'{
    entity_id = EntityId,
    is_hardcore = IsHardcore,
    dimension_names = DimensionNames,
    max_players = MaxPlayers,
    view_distance = ViewDistance,
    simulation_distance = SimulationDistance,
    reduced_debug_info = ReducedDebugInfo,
    enable_respawn_screen = EnableRespawnScreen,
    do_limited_crafting = DoLimitedCrafting,
    dimension_type = DimensionType,
    dimension_name = DimensionName,
    hashed_seed = HashedSeed,
    game_mode = GameMode,
    previous_game_mode = PreviousGameMode,
    is_debug = IsDebug,
    is_flat = IsFlat,
    death_location = DeathLocation,
    portal_cooldown = PortalCooldown,
    sea_level = SeaLevel,
    online_mode = OnlineMode,
    enforces_secure_chat = EnforcesSecureChat
}, State) ->
    {{'minecraft:login', [
        EntityId, IsHardcore, DimensionNames, MaxPlayers, ViewDistance,
        SimulationDistance, ReducedDebugInfo, EnableRespawnScreen, DoLimitedCrafting,
        DimensionType, DimensionName, HashedSeed, GameMode, PreviousGameMode,
        IsDebug, IsFlat, DeathLocation, PortalCooldown, SeaLevel, OnlineMode, EnforcesSecureChat
    ]}, State};
encode_login({'minecraft:login', Fields}, State) ->
    {{'minecraft:login', Fields}, State}.

encode_game_event(#'minecraft:game_event'{event = Event, value = Value}, State) ->
    {{'minecraft:game_event', [Event, Value]}, State};
encode_game_event({'minecraft:game_event', [Event, Value]}, State) ->
    {{'minecraft:game_event', [Event, Value]}, State}.

encode_player_info_update(Record = #'minecraft:player_info_update'{}, State) ->
    {{'minecraft:player_info_update', [Record]}, State};
encode_player_info_update(Record = #player_info_update{}, State) ->
    {{'minecraft:player_info_update', [Record]}, State};
encode_player_info_update({'minecraft:player_info_update', Fields}, State) ->
    {{'minecraft:player_info_update', Fields}, State};
encode_player_info_update({Actions, Players}, State) ->
    {{'minecraft:player_info_update', [{Actions, Players}]}, State}.


encode_player_position(#'minecraft:player_position'{
    teleport_id = TeleportId,
    x = X,
    y = Y,
    z = Z,
    velocity_x = Vx,
    velocity_y = Vy,
    velocity_z = Vz,
    yaw = Yaw,
    pitch = Pitch,
    flags = Flags
}, State) ->
    {{'minecraft:player_position', [TeleportId, X, Y, Z, Vx, Vy, Vz, Yaw, Pitch, Flags]}, State};

encode_player_position({'minecraft:player_position', Fields}, State) ->
    {{'minecraft:player_position', Fields}, State}.

encode_set_chunk_cache_center(#'minecraft:set_chunk_cache_center'{chunk_x = ChunkX, chunk_z = ChunkZ}, State) ->
    NewState = case State of undefined -> undefined; _ -> State#state{current_chunk = {ChunkX, ChunkZ}} end,
    {{'minecraft:set_chunk_cache_center', [ChunkX, ChunkZ]}, NewState};
encode_set_chunk_cache_center({'minecraft:set_chunk_cache_center', [ChunkX, ChunkZ]}, State) ->
    NewState = case State of undefined -> undefined; _ -> State#state{current_chunk = {ChunkX, ChunkZ}} end,
    {{'minecraft:set_chunk_cache_center', [ChunkX, ChunkZ]}, NewState};
encode_set_chunk_cache_center({ChunkX, ChunkZ}, State) ->
    NewState = case State of undefined -> undefined; _ -> State#state{current_chunk = {ChunkX, ChunkZ}} end,
    {{'minecraft:set_chunk_cache_center', [ChunkX, ChunkZ]}, NewState}.

encode_forget_level_chunk(#'minecraft:forget_level_chunk'{chunk_z = ChunkZ, chunk_x = ChunkX}, State) ->
    {{'minecraft:forget_level_chunk', [ChunkZ, ChunkX]}, State};
encode_forget_level_chunk({'minecraft:forget_level_chunk', [ChunkZ, ChunkX]}, State) ->
    {{'minecraft:forget_level_chunk', [ChunkZ, ChunkX]}, State};
encode_forget_level_chunk({ChunkZ, ChunkX}, State) ->
    {{'minecraft:forget_level_chunk', [ChunkZ, ChunkX]}, State}.

encode_level_chunk_with_light(#'minecraft:level_chunk_with_light'{
    chunk_x = X,
    chunk_z = Z,
    heightmaps = Heightmaps,
    data = Data,
    block_entities = BlockEntities,
    light = Light
}, State) ->
    {{'minecraft:level_chunk_with_light', [X, Z, Heightmaps, Data, BlockEntities, Light]}, State};
encode_level_chunk_with_light({'minecraft:level_chunk_with_light', DataList}, State) ->
    {{'minecraft:level_chunk_with_light', DataList}, State}.


