-module(player).

-behaviour(gen_server).

-export([start_link/2, 
        stop/1, 
        init/1, 
        terminate/2, 
        code_change/3,
        handle_cast/2,
        handle_call/3,
        handle_info/2,
        tick/2]).

-include("records.hrl").

-define(SERVER, ?MODULE).

-define(HANDSHAKE, 0).
-define(STATUS, 1).
-define(LOGIN, 2).
-define(TRANSFER, 3).
-define(CONFIGURATION, 4).
-define(PLAY, 5).

-record(state, {listen_pid, listen_socket, player_socket, queue, state_of_play = ?HANDSHAKE, db_key, keep_alive}).

%% API
tick(Pid, Message) ->
    gen_server:cast(Pid, Message).



stop(Name) ->
    gen_server:call(Name, stop).

start_link(Listen_pid, Listen_socket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Listen_pid, Listen_socket], []).

init([Listen_pid, Listen_socket]) ->
    self() ! run_accept,
    {ok, #state{listen_pid = Listen_pid,
            listen_socket = Listen_socket,
            queue = <<>>}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.
    
handle_info(run_accept, State) ->
    {ok, Socket} = gen_tcp:accept(State#state.listen_socket),
    gen_server:cast(State#state.listen_pid, connected),
    {noreply, State#state{player_socket = Socket}};

handle_info(keep_alive, State) ->
    erlang:send_after(5000, self(), keep_alive),
    Random = rand:uniform(1000),
    send_message(Random, serverbound_keep_alive_play, State),
    {noreply, State#state{keep_alive = Random}};

handle_info({tcp, _Socket, Data}, State) ->

    Queue = State#state.queue,
    Data2 =
        if Queue =/= <<>> ->
               <<Queue/binary, Data/binary>>;
           true ->
               Data
        end,
    NewState = 
        case varint:decode_varint(Data2) of
            {error, _} ->
                State#state{queue = Data2};
                
            {_Length, _Data3} ->
                Next_state = State#state{queue = <<>>},
                message(Data2, Next_state)
        end,
    {noreply, NewState}.

message(Data, State) ->
    {Length, Data2} = varint:decode_varint(Data),
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
    {Packet_ID, Data2} = varint:decode_varint(Data),
    case State#state.state_of_play of        
        ?HANDSHAKE ->
            Packet_name = data_packets:get_handshake_packet_name(Packet_ID),
            io:format("~p : ", [Packet_name]),
            io:format("~p~n", [Data]),
            Decoded = decode:decode_message(Data2, Packet_name),
            handle_decoded_message({Packet_name, Decoded}, State);
            
        ?STATUS ->
            Packet_name = data_packets:get_status_packet_name_clientbound(Packet_ID),
            io:format("~p : ", [Packet_name]),
            io:format("~p~n", [Data]),
            Decoded = decode:decode_message(Data2, Packet_name),
            handle_decoded_message({Packet_name, Decoded}, State);
            
        ?LOGIN ->
            Packet_name = data_packets:get_login_packet_name_serverbound(Packet_ID),
            io:format("~p : ", [Packet_name]),
            io:format("~p~n", [Data]),
            Decoded = decode:decode_message(Data2, Packet_name),
            handle_decoded_message({Packet_name, Decoded}, State);
            
        ?CONFIGURATION ->
            Packet_name = data_packets:get_configuration_packet_name_serverbound(Packet_ID),
            io:format("~p : ", [Packet_name]),
            io:format("~p~n", [Data]),
            Decoded = decode:decode_message(Data2, Packet_name),
            handle_decoded_message({Packet_name, Decoded}, State);
        ?PLAY ->
            Packet_name = data_packets:get_play_packet_name_serverbound(Packet_ID),
            io:format("~p : ", [Packet_name]),
            io:format("~p~n", [Data]),
            Decoded = decode:decode_message(Data2, Packet_name),
            handle_decoded_message({Packet_name, Decoded}, State)
    end.



handle_decoded_message(Data, State) ->
    NewState = 
    case Data of
        {handshake, [_Protocol_version, _Server_address, _Server_port, Next_state]} ->
            State#state{state_of_play = Next_state};
        {login_start, [Username, Local_UUID]} ->
            Non_local_UUID = mojang_api:get_player_uuid(Username),
            
            case player_manager:read_from_db(Non_local_UUID) of
                [] ->
                    Player2 = #db_player{uuid = Local_UUID, username = Username, eid = 1, gamemode = 1, coords = {0, 100, 0, 0, 0}, current_slot = 0, dimension = 0},
                    player_manager:write_to_db(Non_local_UUID, Player2);
                _ ->
                    ok
            end,
            
            {Name, Value} = mojang_api:get_profile(Non_local_UUID),
            send_message([Local_UUID, Username, {Name, Value}], login_success, State),
            
            State#state{db_key = Non_local_UUID};
        {encryption_response, [_Shared_secret, _Verify_token]} ->
            State;
        {login_plugin_response, [_Message_id, [_Data]]} ->
            
            State;
        {login_acknowledged, []} ->
            send_message([[["minecraft", "core", "1.21.4"]]], clientbound_known_packs, State),
            State#state{state_of_play = ?CONFIGURATION};
        {cookie_reponse_login, [_Key, [_Payload]]} ->
            State;
        {client_information, [_Locale, _View_distance, _Chat_mode, _Chat_colours, _Displayed_skin_parts, _Main_hand, _Enable_text_filtering, _Allow_server_listings, _Particle_status]} ->
            State;
        {cookie_response_configuration, [_Key, [_Payload]]} ->
            State;
        {serverbound_plugin_message_configuration, [_Channel, _Data]} ->
            State;
        {acknowledge_finish_configuration, []} ->
            Player = player_manager:read_from_db(State#state.db_key),
            send_message(
            [
                Player#db_player.eid, 
                false,%
                [["minecraft:overworld"]],
                configuration:get_configuration(max_players),
                configuration:get_configuration(render_distance),
                configuration:get_configuration(render_distance),
                true,
                true,
                false,
                0,
                "minecraft:overworld",
                <<8,5,2,2,3,2,2,6>>,
                Player#db_player.gamemode,
                Player#db_player.gamemode,
                false,
                true,
                false,
                0,
                20,
                false
            ], login_play, State),
            erlang:send_after(5000, self(), keep_alive),
            State#state{state_of_play = ?PLAY};
        {serverbound_keep_alive_play, [Keep_alive_id]} ->
            if(State#state.keep_alive =/= Keep_alive_id) ->
                stop(self());
            true ->
                State
            end,
            State;
        {serverbound_known_packs, [[_Name_space, _ID, _Version]]} ->
            Directory = code:priv_dir(adamantum),
            {ok, Filenames} = file:list_dir(Directory),
            lists:foreach(fun(File) -> send_registry(State, File) end, Filenames),
            send_registry(State, "trim_material"),
            send_message([], finish_configuration, State),
            State;

%% if type == 2 will match with the second one
        {interact, [_Entity_ID, _Type, _Sneak_key_pressed]} ->
            State;
        {interact, [_Entity_ID, {_Type, _Target_X, _Target_Y, _Target_Z, _Hand}, _Sneak_key_pressed]} ->
            State
        end,
    NewState.

send_message(Data, Packet_name, State) ->
    Message = encode:encode_message(Data, Packet_name),
    Length = varint:encode_varint(byte_size(Message)),
    io:format("sent message : ~p~n", [Message]),
    gen_tcp:send(State#state.player_socket, <<Length/binary, Message/binary>>).











send_registry(State, Name) ->
    Identifier = string:concat("minecraft:", Name),
    {ok, Data} = file:read_file(Filename),
    Decoded = json:decode(Data),
    Data2 = maps:get(list_to_binary(Identifier), Decoded),
    Interator = maps:iterator(Data2),
    Result = parse_registry(Interator, []),
    send_message([Identifier, length(Result), Result], registry_data, State).

parse_registry(Iterator, Acc) ->
    case maps:next(Iterator) of
        {Key, Value, NewIterator} ->
            parse_registry(NewIterator, [[Key, true, json:encode(Value)]|Acc]);
        _ ->
            lists:reverse(Acc)
    end.
    