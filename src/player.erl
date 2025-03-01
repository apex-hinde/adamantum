-module(player).

-behaviour(gen_server).

-export([start_link/2, 
        stop/1, 
        init/1, 
        terminate/2, 
        code_change/3,
        handle_cast/2,
        handle_call/3,
        handle_info/2]).

-include("records.hrl").

-define(SERVER, ?MODULE).

-define(HANDSHAKE, 0).
-define(STATUS, 1).
-define(LOGIN, 2).
-define(TRANSFER, 3).
-define(CONFIGURATION, 4).
-define(PLAY, 5).

-record(state, {listen_pid, listen_socket, player_socket, queue, state_of_play = ?HANDSHAKE, db_key}).

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

handle_cast(Req, State) ->
    NewState =
        case Req of
            {handshake, [_Protocol_version, _Server_address, _Server_port, Next_state]} ->
                State#state{state_of_play = Next_state};
            {login_start, [Username, Local_UUID]} ->
                Non_local_UUID = mojang_api:get_player_uuid(Username),
                case player_manager:read_from_db(Non_local_UUID) of
                    Player = 
                        [] ->
                            Player2 = #db_player{uuid = Local_UUID, username = Username, eid = 1, gamemode = 1, coords = {0, 100, 0, 0, 0}, current_slot = 0, dimension = 0},
                            player_manager:write_to_db(Non_local_UUID, Player2),
                            Player2;
                        Player2 ->
                            Player2
                  end,
                
                {Name, Value} = mojang_api:get_profile(Non_local_UUID),
                encode_message([Local_UUID, Username, {Name, Value}], login_success),


                

                State#state{db_key = Non_local_UUID};
            {encryption_response, [_Shared_secret, _Verify_token]} ->
                State;
            {login_plugin_response, [_Message_id, [_Data]]} ->
                
                State;
            {login_acknowledged, []} ->
                State#state{state_of_play = ?CONFIGURATION};
%% not implimented
            {cookie_reponse_login, [_Key, [_Payload]]} ->
                State;
%% not implimented
            {client_information, [_Locale, _View_distance, _Chat_mode, _Chat_colours, _Displayed_skin_parts, _Main_hand, _Enable_text_filtering, _Allow_server_listings, _Particle_status]} ->
                State;
            {cookie_response_configuration, [_Key, [_Payload]]} ->
                State;
            {serverbound_plugin_message_configuration, [_Channel, _Data]} ->
                encode_message([], finish_configuration),
                State;
            {acknowledge_finish_configuration, []} ->
                State;





            {login_success, Data} ->
                send_message(Data, State),
                State#state{state_of_play = ?CONFIGURATION};
            {finish_configuration, Data} ->
                send_message(Data, State),
                State#state{state_of_play = ?PLAY}

            
            

        end,

    {noreply, NewState}.

handle_info(run_accept, State) ->
    {ok, Socket} = gen_tcp:accept(State#state.listen_socket),
    gen_server:cast(State#state.listen_pid, connected),
    NewState = State#state{player_socket = Socket},
    {noreply, NewState};

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
    io:format("~p~n", [Data]),
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
    NewState =
        case State#state.state_of_play of        
            ?HANDSHAKE ->
                Packet_name = data_packets:get_handshake_packet_name(Packet_ID),
                Decoded = decode:decode_message(Data2, Packet_name),
                gen_server:cast(?SERVER, {Packet_name, Decoded}),
                State#state{state_of_play = ?LOGIN};
            ?STATUS ->
                Packet_name = data_packets:get_status_packet_name_clientbound(Packet_ID),
                Decoded = decode:decode_message(Data2, Packet_name),
                gen_server:cast(?SERVER, {Packet_name, Decoded}),
                State;
            ?LOGIN ->
                Packet_name = data_packets:get_login_packet_name_serverbound(Packet_ID),
                Decoded = decode:decode_message(Data2, Packet_name),
                gen_server:cast(?SERVER, {Packet_name, Decoded}),
                State;
            ?CONFIGURATION ->
                Packet_name = data_packets:get_configuration_packet_name_serverbound(Packet_ID),
                Decoded = decode:decode_message(Data2, Packet_name),
                gen_server:cast(?SERVER, {Packet_name, Decoded}),
                State
    end,
    NewState.


send_message(Message, State) ->
    Length = varint:encode_varint(byte_size(Message)),

    gen_tcp:send(State#state.player_socket, <<Length/binary, Message/binary>>).

encode_message(Data, Packet_name) ->
    {_, Data2} = encode:encode_message(Data, Packet_name),
    gen_server:cast(self(), {Packet_name, Data2}).
    