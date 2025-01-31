-module(adamantum_player).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("records.hrl").
-export([stop/1, start_link/2, init/1, terminate/2, code_change/3, handle_info/2, get_tick/0]).
-export([handle_call/3, handle_cast/2]).
%for state of play, 0 = handshake, 1 = status, 2 = login, 3 = play
 -define(HANDSHAKE, 0).
-define(STATE, 1).
-define(LOGIN, 2).
-define(PLAY, 3).
-record(state, 
        {listen_pid, listen_socket, state_of_play = ?HANDSHAKE, socket, db_key, queue, loaded_chunks}).





stop(Name) ->
    gen_server:call(Name, stop).

start_link(Listen_pid, Listen_socket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Listen_pid, Listen_socket], []).

get_tick() ->
    gen_server:cast(?SERVER, tick).

init([Listen_pid, Listen_socket]) ->
    self() ! run_accept,
    {ok, #state{listen_pid=Listen_pid,
        listen_socket=Listen_socket,
        queue = <<>>, loaded_chunks = []}}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

    
handle_cast(tick, State) ->
    DB_player = adamantum_player_manager:read_from_db(State#state.db_key),
    NewState =
        case adamantum_chunk_manager:update_player_chunks(DB_player#db_player.coords, State#state.loaded_chunks) of
            {update_player_chunks, Chunks_to_remove, Chunks_to_add, Player_chunks_load} ->
                io:format("~p~n", [Chunks_to_add]),
                seperate_chunk_data_into_parts(Chunks_to_remove, Chunks_to_add, DB_player, State),
                State#state{loaded_chunks = Player_chunks_load};

            no_chunk ->
                State
        end,
    {noreply, NewState};



handle_cast(Req, State) ->
    NewState = 
        case Req of 
            {handshake, [_Protocol_version, _Server_address, _Server_port, Next_state]} ->
                State#state{state_of_play = Next_state};
                
            {login_start, [Username]} ->
                UUID = "c534f6a0-7882-3f79-8143-0107ae25aba5",
                DB_player = 
                    case adamantum_player_manager:read_from_db(UUID) of
                        [] ->
                            io:format("~p~n", ["new player"]),
                            Player = #db_player{uuid = UUID, username = Username, gamemode = 1, eid = 330, coords = {0,100,0,0,0}, current_slot = 0, dimension = 0},
                            adamantum_player_manager:write_to_db(UUID, Player),
                            Player;
                        DB ->
                            Player = #db_player{uuid = UUID, username = Username, gamemode = 1, eid = 330, coords = {0,100,0,0,0}, current_slot = 0, dimension = 0},
                            adamantum_player_manager:write_to_db(UUID, Player),
                            Player

                    end,
                encode_message([UUID, binary_to_list(Username)], login_success),
                encode_message([DB_player#db_player.eid, DB_player#db_player.gamemode, DB_player#db_player.dimension, 2, 20, "flat", false], join_game),
                {X,Y,Z,Yaw,Pitch} = DB_player#db_player.coords,
                encode_message([{X, Y, Z}], spawn_position),
                encode_message([X, Y, Z,Yaw,Pitch,0], player_position_and_look),


                adamantum_player_manager:connect_to_PM(UUID, self()),
                State#state{state_of_play = 3, db_key = UUID};

            {keep_alive, [_Keep_alive_id]} ->
                State;
            {chat_message, [_Chat_message]} ->
                State;
            {use_entity, [_Target, 0]} ->
                State;
            {use_entity, [_Target, 1]} ->
                State;
            {use_entity, [_Target, 2, _Target_X, _Target_Y, _Target_Z]} ->
                State;

    %player movement
            {player, [_On_ground]} ->
                State;
            {player_position, [X, Y, Z, _On_ground]} ->
                Db_player = adamantum_player_manager:read_from_db(State#state.db_key),
                {_, _, _, Yaw, Pitch} = Db_player#db_player.coords,
                adamantum_player_manager:write_to_db(State#state.db_key, Db_player#db_player{coords = {X, Y, Z, Yaw, Pitch}}),
                State;
            {player_look, [Yaw, Pitch, _On_ground]} ->
                Db_player = adamantum_player_manager:read_from_db(State#state.db_key),
                {X, Y, Z, _, _} = Db_player#db_player.coords,
                adamantum_player_manager:write_to_db(State#state.db_key, Db_player#db_player{coords = {X, Y, Z, Yaw, Pitch}}),
                State;
            {player_position_and_look, [X, Y, Z, Yaw, Pitch, _On_ground]} ->
                Db_player = adamantum_player_manager:read_from_db(State#state.db_key),
                adamantum_player_manager:write_to_db(State#state.db_key, Db_player#db_player{coords = {X, Y, Z, Yaw, Pitch}}),
                State;
    %player actions
            {player_digging, [_Status, {_X, _Y, _Z}, _Face]} ->
                State;
            {player_block_placement, [{_X, _Y, _Z}, _Face, _Cursor_X, _Cursor_Y, _Cursor_Z]} ->
                State;
            {held_item_change, [_Slot]} ->
                State;
            {animation, []} ->
                State;
            {entity_action, [_Entity_id, _Action_id, _Action_parameter]} ->
                State;
            {steer_vehicle, [_Sideways, _Forward, _Flags]} ->
                State;
            {close_window, [_Window_id]} ->
                State;
            {click_window, [_Window_id, _Slot, _Button, _Action_number, _Mode, _Clicked_item]} ->
                State;
            {confirm_transaction, [_Window_id, _Action_number, _Accepted]} ->
                State;
            {creative_inventory_action, [_Slot, _Clicked_item]} ->
                State;
            {enchant_item, [_Window_id, _Enchantment]} ->
                State;
            {update_sign, [{_X, _Y, _Z}, _Line_1, _Line_2, _Line_3, _Line_4]} ->
                State;
            {player_abilities, [_Flags, _Flying_speed, _Walking_speed]} ->
                State;
            {tab_complete, [_Text, _Has_position]} ->
                State;
            {tab_complete, [_Text, _Has_position, {_X, _Y, _Z}]} ->
                State;
            {client_settings, [_Locale, _View_distance, _Chat_mode, _Chat_colors, _Displayed_skin_parts]} ->
                State;
            {client_status, [_Action_id]} ->
                State;
            {plugin_message, [_Channel, _Data]} ->
                State;
            {spectate, [_Target_player]} ->
                State;
            {resource_pack_status, [_Hash, _Result]} ->
                State;

    %everything after this point is clientbound and kinda set up
            {disconnect, Msg} -> %need to send player reason for disconnect
                send_message(Msg, State),
                State;
            {encryption_request, Msg} ->
                send_message(Msg, State),
                State;
            {login_success, Msg} ->
                send_message(Msg, State),
                State;
            {set_compression, Msg} ->
                send_message(Msg, State),
                State;
            {join_game, Msg} ->
                send_message(Msg, State),
                State;
            {spawn_position, Msg} ->
                send_message(Msg, State),
                State;
            {player_position_and_look, Msg} ->
                send_message(Msg, State),
                State;
            {chunk_data, Msg} ->
                send_message(Msg, State),
                State
        end,
    {noreply, NewState}.




handle_info(run_accept, State) ->
    {ok, Socket} = gen_tcp:accept(State#state.listen_socket),
    gen_server:cast(State#state.listen_pid, connected),
    NewState = State#state{socket = Socket},
    {noreply, NewState};

handle_info({tcp, _Socket, Data}, State) ->
    Queue = State#state.queue,
    Data2 = 
        if Queue =/= <<>> ->
            <<Queue/binary, Data/binary>>; 
        true ->
            Data
        end,

    case varint:decode_varint(Data2) of
        {error, _} ->
            Next_state = State#state{queue = Data2},
            {noreply, Next_state};
        {_Length, _Data3} ->
            Next_state = State#state{queue = <<>>},
            NewState = message(Data2, Next_state),
            {noreply, NewState}

    end.
    
message(Data, State) ->
    {Length, Data2} = varint:decode_varint(Data),
    Length_of_data = byte_size(Data2),

    if Length =:= Length_of_data ->
            process_message(Data2, State);

       Length < Length_of_data ->
            <<Interim_data:Length/binary, Rest/binary>> = Data2,
            NewState = process_message(Interim_data, State),

            message(Rest, NewState);

        Length > Length_of_data ->
            State#state{queue = Data}

       end.
    
process_message(Data,  State) ->
    {Packet_ID, Data2} = varint:decode_varint(Data),
    NewState = 
        case State#state.state_of_play of
            ?HANDSHAKE ->
                Packet_name = data_packets:get_handshake_packet_name(Packet_ID),
                Decoded = adamantum_decode:decode_message(Data2, Packet_name),
                gen_server:cast(self(), {Packet_name, Decoded}),
                State#state{state_of_play = ?LOGIN};
                
            ?LOGIN ->
                Packet_name = data_packets:get_login_packet_name_serverbound(Packet_ID),
                Decoded = adamantum_decode:decode_message(Data2, Packet_name),
                gen_server:cast(self(), {Packet_name, Decoded}),
                State#state{state_of_play = ?PLAY};
                
            ?PLAY ->
                Packet_name = data_packets:get_play_packet_name_serverbound(Packet_ID),
                Decoded = adamantum_decode:decode_message(Data2, Packet_name),
                gen_server:cast(self(), {Packet_name, Decoded}),
                State
                
    end,
    NewState.





send_message({_Packet_name, Message}, State) ->
    Length = varint:encode_varint(byte_size(Message)),

    gen_tcp:send(State#state.socket, <<Length/binary, Message/binary>>).


encode_message(Data, Packet_name) ->
    Data2 = adamantum_decode:encode_message(Data, Packet_name),

    gen_server:cast(self(), {Packet_name, Data2}),
    ok.

seperate_chunk_data_into_parts([], [], _DB_player, _State) ->
    ok;

seperate_chunk_data_into_parts([], Chunks_to_add, DB_player, State) ->
    {Chunks_to_add2, Rest} = 
        if length(Chunks_to_add) =< 4
            -> {Chunks_to_add, []};
        true ->
            lists:split(4, Chunks_to_add)
        end,

    

    Chunks_to_send = adamantum_encode:encode_chunk_bulk([], Chunks_to_add2, <<>>, <<>>), 
    Number_of_chunks = varint:encode_varint(length(Chunks_to_add2)),
    io:format("Number of send: ~p~n", [Number_of_chunks]),
    case DB_player#db_player.dimension of
        0 ->
            send_message({map_chunk_bulk, <<38:8, 1:8, Number_of_chunks/binary, Chunks_to_send/binary>>}, State); 
        _ ->
            send_message({map_chunk_bulk, <<38:8, 0:8, Number_of_chunks/binary, Chunks_to_send/binary>>}, State)
    end,
    seperate_chunk_data_into_parts([], Rest, DB_player, State);

seperate_chunk_data_into_parts(Chunks_to_remove, Chunks_to_add, DB_player, State) ->

    {Chunks_to_remove2, Rest} = 
        if length(Chunks_to_remove) =< 4
            -> {Chunks_to_remove, []};
        true ->
            lists:split(4, Chunks_to_remove)
        end,

    Chunks_to_send = adamantum_encode:encode_chunk_bulk(Chunks_to_remove2, [], <<>>, <<>>), 
    Number_of_chunks = varint:encode_varint(length(Chunks_to_remove2)),
    io:format("Number of remove: ~p~n", [Number_of_chunks]),

    case DB_player#db_player.dimension of
        0 ->
            send_message({map_chunk_bulk, <<38:8, 1:8, Number_of_chunks/binary, Chunks_to_send/binary>>}, State); 
        _ ->
            send_message({map_chunk_bulk, <<38:8, 0:8, Number_of_chunks/binary, Chunks_to_send/binary>>}, State)
    end,
    seperate_chunk_data_into_parts(Rest, Chunks_to_add, DB_player, State).
