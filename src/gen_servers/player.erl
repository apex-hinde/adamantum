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
    username,
    keep_alive,
    teleport_id,
    chunk_managers = [],
    render_distance = 12,
    current_chunk = {0,0},
    player_data,
    inventories,
    current_slot,
    entity_id,
    pending_state_updates = #{},
    pending_events = [],
    loaded_entities = sets:new()
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
    gen_server:start_link(?MODULE, [Listen_pid, Listen_socket], []).

init([Listen_pid, Listen_socket]) ->
    self() ! run_accept,
    {ok, #state{listen_pid = Listen_pid,
            listen_socket = Listen_socket,
            queue = <<>>,
            state_of_play = ?HANDSHAKE,
            player_data = #player_position{},
            loaded_entities = sets:new(),
            current_slot = 0}}.

terminate(_Reason, #state{current_chunk = {ChunkX, ChunkZ}, entity_id = EntityId, render_distance = R}) when is_integer(EntityId) ->
    ChunkCoords = [{X, Z} || X <- lists:seq(ChunkX - R, ChunkX + R), Z <- lists:seq(ChunkZ - R, ChunkZ + R)],
    SelfPid = self(),
    lists:foreach(fun({CX, CZ}) ->
        NearbyPids = pg:get_members(minecraft_players, {chunk, CX, CZ}),
        [Pid ! {nearby_event, {event, player_left_view, {SelfPid, EntityId}}} || Pid <- NearbyPids, Pid /= SelfPid]
    end, ChunkCoords),
    ok;
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
    {stop, normal, State};

handle_info({nearby_event, Event}, State) ->
    handle_nearby_event(Event, State);

handle_info(tick, State = #state{state_of_play = ?PLAY}) ->
    erlang:send_after(50, self(), tick),
    StateUpdates = maps:values(State#state.pending_state_updates),
    Events = State#state.pending_events,
    AllPackets = StateUpdates ++ Events,
    case AllPackets of
        [] -> 
            {noreply, State};
        _ ->
            {BatchBinaries, FinalState} = lists:foldl(
                fun({PacketName, Record}, {BinAcc, CurrentState}) ->
                    {Encoded, NextState} = encode_message(PacketName, Record, CurrentState),
                    Message = encode:encode_message(Encoded, PacketName, CurrentState#state.state_of_play),
                    Length = encode:encode_type(byte_size(Message), varint),
                    Frame = <<Length/binary, Message/binary>>,
                    {[Frame | BinAcc], NextState}
                end,
                {[], State},
                AllPackets
            ),
            gen_tcp:send(State#state.player_socket, iolist_to_binary(lists:reverse(BatchBinaries))),
            {noreply, FinalState#state{pending_state_updates = #{}, pending_events = []}}
    end;
handle_info(tick, State) ->
    erlang:send_after(50, self(), tick),
    {noreply, State}.



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
        {'minecraft:player_input', Record} ->
            minecraft_player_input(Record, State);
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

    {ok, NewState#state{
        uuid = Record#'minecraft:hello_serverbound'.uuid,
        username = Record#'minecraft:hello_serverbound'.name
    }}.
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
    EntityId = erlang:unique_integer([positive]),
    NewState = State#state{state_of_play = ?PLAY, entity_id = EntityId},
    self() ! tick,
    LoginPacket = #'minecraft:login'{
        entity_id = EntityId,
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
        game_mode = 0,
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
    Username = case NewState4#state.username of undefined -> "Player"; U -> U end,
    PlayerInfo = #'minecraft:player_info_update'{
        actions = 1,
        players = [{NewState4#state.uuid, [{add_player, Username, []}]}]
    },
    {ok, NewState5} = player:send_message('minecraft:player_info_update', PlayerInfo, NewState4),
    {ok, NewState6} = player:send_message('minecraft:set_chunk_cache_center', #'minecraft:set_chunk_cache_center'{chunk_x = 0, chunk_z = 0}, NewState5),
    {ok, NewState7} = send_surrounding_chunks(0, 0, 12, NewState6),
    ChunkX = pos_to_chunk_coord(NewState4#state.player_data#player_position.x),
    ChunkZ = pos_to_chunk_coord(NewState4#state.player_data#player_position.z),

    InitialChunks = [{X, Z} || 
        X <- lists:seq(
            ChunkX - NewState4#state.render_distance,
            ChunkX + NewState4#state.render_distance), 
        Z <- lists:seq(
            ChunkZ - NewState4#state.render_distance, 
            ChunkZ + NewState4#state.render_distance)],
    lists:foreach(fun({X, Z}) ->
        pg:join(minecraft_players, {chunk, X, Z}, self()),
        broadcast_to_nearby(X, Z, {event, player_entered_view, {self(), NewState7#state.entity_id, NewState7#state.uuid, Username, NewState7#state.player_data}})
    end, InitialChunks),
    
    {ok, NewState7}.

minecraft_accept_teleportation(Record, State) ->
    _TeleportId = Record#'minecraft:accept_teleportation'.teleport_id,
    {ok, State}.
minecraft_move_player_pos(Record, State) ->
    CurrentPos = State#state.player_data,
    Flags = Record#'minecraft:move_player_pos'.flags,
    NewX = Record#'minecraft:move_player_pos'.x,
    NewY = Record#'minecraft:move_player_pos'.y,
    NewZ = Record#'minecraft:move_player_pos'.z,
    OnGround = (Flags band 16#01) =/= 0,

    NewPos = CurrentPos#player_position{
        x = NewX,
        y = NewY,
        z = NewZ,
        on_ground = OnGround,
        touching_wall = (Flags band 16#02) =/= 0
    },
    {ok, State2} = check_and_update_chunks(NewX, NewZ, State),

    DeltaX = trunc((NewX * 32.0 - CurrentPos#player_position.x * 32.0) * 128.0),
    DeltaY = trunc((NewY * 32.0 - CurrentPos#player_position.y * 32.0) * 128.0),
    DeltaZ = trunc((NewZ * 32.0 - CurrentPos#player_position.z * 32.0) * 128.0),

    ChunkX = pos_to_chunk_coord(NewX),
    ChunkZ = pos_to_chunk_coord(NewZ),
    IsTooFar = (DeltaX > 32767) orelse (DeltaX < -32768) orelse
               (DeltaY > 32767) orelse (DeltaY < -32768) orelse
               (DeltaZ > 32767) orelse (DeltaZ < -32768),
    if
        IsTooFar ->
            TeleportRecord = #'minecraft:teleport_entity'{
                entity_id = State2#state.entity_id,
                x = NewX, y = NewY, z = NewZ,
                vx = 0.0, vy = 0.0, vz = 0.0,
                yaw = CurrentPos#player_position.yaw,
                pitch = CurrentPos#player_position.pitch,
                flags = 0, on_ground = OnGround
            },
            broadcast_to_nearby(ChunkX, ChunkZ, {state_update, State2#state.entity_id, 'minecraft:teleport_entity', TeleportRecord});
        true ->
            MoveRecord = #'minecraft:move_entity_pos'{
                entity_id = State2#state.entity_id,
                delta_x = DeltaX,
                delta_y = DeltaY,
                delta_z = DeltaZ,
                on_ground = OnGround
            },
            broadcast_to_nearby(ChunkX, ChunkZ, {state_update, State2#state.entity_id, 'minecraft:move_entity_pos', MoveRecord})
    end,

    {ok, State2#state{player_data = NewPos}}.

minecraft_move_player_pos_rot(Record, State) ->
    CurrentPos = State#state.player_data,
    Flags = Record#'minecraft:move_player_pos_rot'.flags,
    NewX = Record#'minecraft:move_player_pos_rot'.x,
    NewY = Record#'minecraft:move_player_pos_rot'.y,
    NewZ = Record#'minecraft:move_player_pos_rot'.z,
    Yaw = Record#'minecraft:move_player_pos_rot'.yaw,
    Pitch = Record#'minecraft:move_player_pos_rot'.pitch,
    OnGround = (Flags band 16#01) =/= 0,

    NewPos = CurrentPos#player_position{
        x = NewX,
        y = NewY,
        z = NewZ,
        yaw = Yaw,
        pitch = Pitch,
        on_ground = OnGround,
        touching_wall = (Flags band 16#02) =/= 0
    },
    {ok, State2} = check_and_update_chunks(NewX, NewZ, State),

    DeltaX = trunc((NewX * 32.0 - CurrentPos#player_position.x * 32.0) * 128.0),
    DeltaY = trunc((NewY * 32.0 - CurrentPos#player_position.y * 32.0) * 128.0),
    DeltaZ = trunc((NewZ * 32.0 - CurrentPos#player_position.z * 32.0) * 128.0),

    ChunkX = pos_to_chunk_coord(NewX),
    ChunkZ = pos_to_chunk_coord(NewZ),
    IsTooFar = (DeltaX > 32767) orelse (DeltaX < -32768) orelse
               (DeltaY > 32767) orelse (DeltaY < -32768) orelse
               (DeltaZ > 32767) orelse (DeltaZ < -32768),
    if
        IsTooFar ->
            TeleportRecord = #'minecraft:teleport_entity'{
                entity_id = State2#state.entity_id,
                x = NewX, y = NewY, z = NewZ,
                vx = 0.0, vy = 0.0, vz = 0.0,
                yaw = Yaw, pitch = Pitch,
                flags = 0, on_ground = OnGround
            },
            broadcast_to_nearby(ChunkX, ChunkZ, {state_update, State2#state.entity_id, 'minecraft:teleport_entity', TeleportRecord});
        true ->
            MoveRecord = #'minecraft:move_entity_pos_rot'{
                entity_id = State2#state.entity_id,
                delta_x = DeltaX,
                delta_y = DeltaY,
                delta_z = DeltaZ,
                yaw = Yaw,
                pitch = Pitch,
                on_ground = OnGround
            },
            broadcast_to_nearby(ChunkX, ChunkZ, {state_update, State2#state.entity_id, 'minecraft:move_entity_pos_rot', MoveRecord})
    end,

    HeadRecord = #'minecraft:rotate_head'{
        entity_id = State2#state.entity_id,
        head_yaw = Yaw
    },
    broadcast_to_nearby(ChunkX, ChunkZ, {state_update, State2#state.entity_id, 'minecraft:rotate_head', HeadRecord}),

    {ok, State2#state{player_data = NewPos}}.

minecraft_move_player_rot(Record, State) ->
    CurrentPos = State#state.player_data,
    Flags = Record#'minecraft:move_player_rot'.flags,
    Yaw = Record#'minecraft:move_player_rot'.yaw,
    Pitch = Record#'minecraft:move_player_rot'.pitch,
    OnGround = (Flags band 16#01) =/= 0,

    NewPos = CurrentPos#player_position{
        yaw = Yaw,
        pitch = Pitch,
        on_ground = OnGround,
        touching_wall = (Flags band 16#02) =/= 0
    },

    MoveRecord = #'minecraft:move_entity_rot'{
        entity_id = State#state.entity_id,
        yaw = Yaw,
        pitch = Pitch,
        on_ground = OnGround
    },

    ChunkX = pos_to_chunk_coord(CurrentPos#player_position.x),
    ChunkZ = pos_to_chunk_coord(CurrentPos#player_position.z),
    broadcast_to_nearby(ChunkX, ChunkZ, {state_update, State#state.entity_id, 'minecraft:move_entity_rot', MoveRecord}),
    HeadRecord = #'minecraft:rotate_head'{
        entity_id = State#state.entity_id,
        head_yaw = Yaw
    },
    broadcast_to_nearby(ChunkX, ChunkZ, {state_update, State#state.entity_id, 'minecraft:rotate_head', HeadRecord}),

    {ok, State#state{player_data = NewPos}}.

minecraft_player_input(_Record, State) ->
    {ok, State}.

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
    lists:foreach(fun({UnloadX, UnloadZ}) ->
        Pids = pg:get_members(minecraft_players, {chunk, UnloadX, UnloadZ}),
        SelfPid = self(),
        [begin
            Pid ! {nearby_event, {event, player_left_view_if_far, {SelfPid, State#state.entity_id, NewChunk, R}}}
         end || Pid <- Pids, Pid /= SelfPid],
        pg:leave(minecraft_players, {chunk, UnloadX, UnloadZ}, self())
    end, ToUnload),
    Username = case State#state.username of undefined -> "Player"; U -> U end,
    lists:foreach(fun({LoadX, LoadZ}) ->
        pg:join(minecraft_players, {chunk, LoadX, LoadZ}, self()),
        broadcast_to_nearby(LoadX, LoadZ, {event, player_entered_view, {self(), State#state.entity_id, State#state.uuid, Username, State#state.player_data}})
    end, ToLoad),

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
    {ok, State4#state{current_chunk = NewChunk}}.




    
    

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



broadcast_to_nearby(ChunkX, ChunkZ, EventMsg) ->
    NearbyPids = pg:get_members(minecraft_players, {chunk, ChunkX, ChunkZ}),
    SelfPid = self(),
    [Pid ! {nearby_event, EventMsg} || Pid <- NearbyPids, Pid /= SelfPid].





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
            encode_level_chunk_with_light(Record, State);
        'minecraft:move_entity_pos_rot' ->
            encode_move_entity_pos_rot(Record, State);
        'minecraft:move_entity_pos' ->
            encode_move_entity_pos(Record, State);
        'minecraft:move_entity_rot' ->
            encode_move_entity_rot(Record, State);
        'minecraft:add_entity' ->
            encode_add_entity(Record, State);
        add_entity ->
            encode_add_entity(Record, State);
        'minecraft:remove_entities' ->
            encode_remove_entities(Record, State);
        remove_entities ->
            encode_remove_entities(Record, State);
        'minecraft:rotate_head' ->
            encode_rotate_head(Record, State);
        rotate_head ->
            encode_rotate_head(Record, State);
        'minecraft:teleport_entity' ->
            encode_teleport_entity(Record, State);
        teleport_entity ->
            encode_teleport_entity(Record, State)
    end.

encode_teleport_entity(#'minecraft:teleport_entity'{
    entity_id = EntityId,
    x = X, y = Y, z = Z,
    vx = Vx, vy = Vy, vz = Vz,
    yaw = Yaw, pitch = Pitch,
    flags = Flags, on_ground = OnGround
}, State) ->
    {{'minecraft:teleport_entity', [EntityId, X, Y, Z, Vx, Vy, Vz, Yaw, Pitch, Flags, OnGround]}, State};
encode_teleport_entity({'minecraft:teleport_entity', DataList}, State) ->
    {{'minecraft:teleport_entity', DataList}, State}.

encode_rotate_head(#'minecraft:rotate_head'{entity_id = EntityId, head_yaw = HeadYaw}, State) ->
    {{'minecraft:rotate_head', [EntityId, HeadYaw]}, State};
encode_rotate_head({'minecraft:rotate_head', [EntityId, HeadYaw]}, State) ->
    {{'minecraft:rotate_head', [EntityId, HeadYaw]}, State}.

encode_remove_entities(#'minecraft:remove_entities'{entity_ids = EntityIds}, State) ->
    {{'minecraft:remove_entities', [EntityIds]}, State};
encode_remove_entities(EntityIds, State) when is_list(EntityIds) ->
    {{'minecraft:remove_entities', [EntityIds]}, State}.

encode_add_entity(Record, State) ->
    {{'minecraft:add_entity', [
        Record#'minecraft:add_entity'.entity_id,
        Record#'minecraft:add_entity'.uuid,
        Record#'minecraft:add_entity'.type,
        Record#'minecraft:add_entity'.x,
        Record#'minecraft:add_entity'.y,
        Record#'minecraft:add_entity'.z,
        Record#'minecraft:add_entity'.velocity,
        Record#'minecraft:add_entity'.pitch,
        Record#'minecraft:add_entity'.yaw,
        Record#'minecraft:add_entity'.head_yaw,
        Record#'minecraft:add_entity'.data
    ]}, State}.

encode_move_entity_pos_rot(Record, State) ->
    {{'minecraft:move_entity_pos_rot', [
        Record#'minecraft:move_entity_pos_rot'.entity_id,
        Record#'minecraft:move_entity_pos_rot'.delta_x,
        Record#'minecraft:move_entity_pos_rot'.delta_y,
        Record#'minecraft:move_entity_pos_rot'.delta_z,
        Record#'minecraft:move_entity_pos_rot'.yaw,
        Record#'minecraft:move_entity_pos_rot'.pitch,
        Record#'minecraft:move_entity_pos_rot'.on_ground
    ]}, State}.

encode_move_entity_pos(Record, State) ->
    {{'minecraft:move_entity_pos', [
        Record#'minecraft:move_entity_pos'.entity_id,
        Record#'minecraft:move_entity_pos'.delta_x,
        Record#'minecraft:move_entity_pos'.delta_y,
        Record#'minecraft:move_entity_pos'.delta_z,
        Record#'minecraft:move_entity_pos'.on_ground
    ]}, State}.

encode_move_entity_rot(Record, State) ->
    {{'minecraft:move_entity_rot', [
        Record#'minecraft:move_entity_rot'.entity_id,
        Record#'minecraft:move_entity_rot'.yaw,
        Record#'minecraft:move_entity_rot'.pitch,
        Record#'minecraft:move_entity_rot'.on_ground
    ]}, State}.




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

build_add_entity_record(EntityId, UUID, #player_position{x = X, y = Y, z = Z, yaw = Yaw, pitch = Pitch}) ->
    UUIDVal = case UUID of
        undefined -> <<0:128>>;
        B when is_binary(B) -> B;
        _ -> <<0:128>>
    end,
    #'minecraft:add_entity'{
        entity_id = EntityId,
        uuid = UUIDVal,
        type = 156,
        x = X,
        y = Y,
        z = Z,
        velocity = #lp_vec3{x = 0.0, y = 0.0, z = 0.0},
        pitch = Pitch,
        yaw = Yaw,
        head_yaw = Yaw,
        data = 0
    }.

handle_nearby_event({state_update, EntityId, MsgType, Record}, State) ->
    Key = {EntityId, MsgType},
    NewStateUpdates = maps:put(Key, {MsgType, Record}, State#state.pending_state_updates),
    {noreply, State#state{pending_state_updates = NewStateUpdates}};

handle_nearby_event({event, player_entered_view, {SenderPid, OtherEntityId, OtherUUID, OtherUsername, OtherPos}}, State) ->
    MyUsername = case State#state.username of undefined -> "Player"; U -> U end,
    case sets:is_element(OtherEntityId, State#state.loaded_entities) of
        true ->
            SenderPid ! {nearby_event, {event, spawn_existing_player, {self(), State#state.entity_id, State#state.uuid, MyUsername, State#state.player_data}}},
            {noreply, State};
        false ->
            PlayerInfoRec = #'minecraft:player_info_update'{
                actions = 1,
                players = [{OtherUUID, [{add_player, OtherUsername, []}]}]
            },
            {ok, State1} = player:send_message('minecraft:player_info_update', PlayerInfoRec, State),
            AddEntityRec = build_add_entity_record(OtherEntityId, OtherUUID, OtherPos),
            {ok, State2} = player:send_message('minecraft:add_entity', AddEntityRec, State1),
            NewLoaded = sets:add_element(OtherEntityId, State2#state.loaded_entities),
            SenderPid ! {nearby_event, {event, spawn_existing_player, {self(), State#state.entity_id, State#state.uuid, MyUsername, State#state.player_data}}},
            {noreply, State2#state{loaded_entities = NewLoaded}}
    end;

handle_nearby_event({event, spawn_existing_player, {_SenderPid, OtherEntityId, OtherUUID, OtherUsername, OtherPos}}, State) ->
    case sets:is_element(OtherEntityId, State#state.loaded_entities) of
        true ->
            {noreply, State};
        false ->
            PlayerInfoRec = #'minecraft:player_info_update'{
                actions = 1,
                players = [{OtherUUID, [{add_player, OtherUsername, []}]}]
            },
            {ok, State1} = player:send_message('minecraft:player_info_update', PlayerInfoRec, State),
            AddEntityRec = build_add_entity_record(OtherEntityId, OtherUUID, OtherPos),
            {ok, State2} = player:send_message('minecraft:add_entity', AddEntityRec, State1),
            NewLoaded = sets:add_element(OtherEntityId, State2#state.loaded_entities),
            {noreply, State2#state{loaded_entities = NewLoaded}}
    end;

handle_nearby_event({event, player_left_view_if_far, {_SenderPid, OtherEntityId, OtherNewChunk, SenderRenderDist}}, State) ->
    {MyCX, MyCZ} = State#state.current_chunk,
    {OtherCX, OtherCZ} = OtherNewChunk,
    DistX = abs(MyCX - OtherCX),
    DistZ = abs(MyCZ - OtherCZ),
    IsFar = (DistX > SenderRenderDist) orelse (DistZ > SenderRenderDist) orelse (DistX > State#state.render_distance) orelse (DistZ > State#state.render_distance),
    case IsFar of
        true ->
            case sets:is_element(OtherEntityId, State#state.loaded_entities) of
                true ->
                    RemoveRec = #'minecraft:remove_entities'{entity_ids = [OtherEntityId]},
                    {ok, State2} = player:send_message('minecraft:remove_entities', RemoveRec, State),
                    NewLoaded = sets:del_element(OtherEntityId, State2#state.loaded_entities),
                    {noreply, State2#state{loaded_entities = NewLoaded}};
                false ->
                    {noreply, State}
            end;
        false ->
            {noreply, State}
    end;

handle_nearby_event({event, player_left_view, {_SenderPid, OtherEntityId}}, State) ->
    case sets:is_element(OtherEntityId, State#state.loaded_entities) of
        true ->
            RemoveRec = #'minecraft:remove_entities'{entity_ids = [OtherEntityId]},
            {ok, State2} = player:send_message('minecraft:remove_entities', RemoveRec, State),
            NewLoaded = sets:del_element(OtherEntityId, State2#state.loaded_entities),
            {noreply, State2#state{loaded_entities = NewLoaded}};
        false ->
            {noreply, State}
    end;

handle_nearby_event({event, PacketName, Record}, State) ->
    NewEvents = State#state.pending_events ++ [{PacketName, Record}],
    {noreply, State#state{pending_events = NewEvents}}.



