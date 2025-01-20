-module(adamantum_player_manager).
-behaviour(gen_server).
-include("records.hrl").
-export([stop/1,start_link/2,init/1,terminate/2,code_change/3, setup/0, clear_player_table/0, handle_info/2]).
-export([handle_call/3,handle_cast/2]).
%for state of play, 0 = handshake, 1 = status, 2 = login, 3 = play
-record(state, 
        {listen_pid, listen_socket, state_of_play = 0, socket, db_key}).
-record(db_mnesia_player, {uuid, data}).


setup() ->
    mnesia:create_table(db_mnesia_player, [{attributes, record_info(fields, db_mnesia_player)},
                                 {type, set}, {disc_copies, [node()]}]).
clear_player_table() ->
    mnesia:clear_table(db_mnesia_player).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Listen_pid, Listen_socket) ->
    gen_server:start_link(?MODULE, [Listen_pid, Listen_socket], []).

init([Listen_pid, Listen_socket]) ->
    self() ! run_accept,
    {ok, #state{listen_pid=Listen_pid,
        listen_socket=Listen_socket}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(Req, State) ->
    io:format("~p~n", ["hi"]),
    case Req of 
        {handshake, [_Protocol_version, _Server_address, _Server_port, Next_state]} ->
            NewState = State#state{state_of_play = Next_state},
            {noreply, NewState};
        {login_start, [Username]} ->
            UUID = "c534f6a0-7882-3f79-8143-0107ae25aba5",
            Player = #db_player{uuid = UUID, username = Username, gamemode = 0, eid = <<0,0,1,74>>, dimension = 0},
            case read_from_db(UUID) of
                [] ->
                    write_to_db(UUID, Player);
                _ ->
                    ok
            end,
            encode_message([UUID, Username], login_success),
            [DB_mnesia] = read_from_db(UUID),
            Player_info = DB_mnesia#db_mnesia_player.data,
            encode_message([Player_info#db_player.eid, Player_info#db_player.gamemode, Player_info#db_player.dimension, 2, 20, flat, false], join_game),
            NewState = State#state{state_of_play = 3, db_key = UUID},
            {noreply, NewState};
        {keep_alive, [_Keep_alive_id]} ->
            {noreply, State};
        {chat_message, [_Chat_message]} ->
            {noreply, State};
        {use_entity, [_Target, 0]} ->
            {noreply, State};
        {use_entity, [_Target, 1]} ->
            {noreply, State};
        {use_entity, [_Target, 2, _Target_X, _Target_Y, _Target_Z]} ->
            {noreply, State};

%everything after this point is clientbound and kinda set up
        {disconnect, Msg} -> %need to send player reason for disconnect
        send_message(Msg, State),
            {noreply, State};
        {encryption_request, Msg} ->
            send_message(Msg, State),
            {noreply, State};
        {login_success, Msg} ->
            send_message(Msg, State),
            {noreply, State};
        {set_compression, Msg} ->
            send_message(Msg, State),
            {noreply, State};
        {join_game, Msg} ->
            send_message(Msg, State),
            {noreply, State}
        
    end.

handle_info(run_accept, State) ->
    {ok, Socket} = gen_tcp:accept(State#state.listen_socket),
    gen_server:cast(State#state.listen_pid, connected),
    NewState = State#state{socket = Socket},
    {noreply, NewState};


handle_info({tcp, _Socket, Data}, State) ->
    {_Length, Data2} = varint:decode_varint(Data),
    {PacketID, Data3} = varint:decode_varint(Data2),

    case State#state.state_of_play of
        0 ->
            Packet_name = data_packets:get_handshake_packet_name(PacketID),
            Decoded = adamantum_decode:decode_message(Data3, Packet_name),
            gen_server:cast(?MODULE, {Packet_name, Decoded}),
            io:format("~p~n", [Packet_name]),
            NewState = State#state{state_of_play = 2},
            {noreply, NewState};
        2 ->
            Packet_name = data_packets:get_login_packet_name_serverbound(PacketID),
            Decoded = adamantum_decode:decode_message(Data3, Packet_name),
            gen_server:cast(?MODULE, {Packet_name, Decoded}),
            NewState = State#state{state_of_play = 3},
            {noreply, NewState};
        3 ->
            Packet_name = data_packets:get_play_packet_name_serverbound(PacketID),

            Decoded = adamantum_decode:decode_message(Data3, Packet_name),
            gen_server:cast(?MODULE, {Packet_name, Decoded}),
            {noreply, State}
    end.

%remove this
login(Data, _Len, State) ->
    UUID = <<"c534f6a0-7882-3f79-8143-0107ae25aba5">>,
    Eid = <<0,0,1,74>>,
    {Len_of_username, Username} = varint:decode_varint(Data),
%    Message_set_compression = adamantum_encode:set_compression(),
    Message_join_game = adamantum_encode:join_game(Eid),
    Message_spawn_position = adamantum_encode:spawn_position(0,0,100),
    Message_player_position_and_look = adamantum_encode:player_position_and_look(0,100,0,0,0,0),
%    Message_chunk_data = adamantum_encode:chunk_data({0,0}),
%    send_message(Message_set_compression, State),
    send_message(Message_join_game, State),
    send_message(Message_spawn_position, State),
    send_message(Message_player_position_and_look, State),
 %   send_message(Message_chunk_data, State),


    
                                
        State.


send_message(Message, State) ->
    Length = varint:encode_varint(byte_size(Message)),

    gen_tcp:send(State#state.socket, <<Length/binary, Message/binary>>).

write_to_db(UUID, Data) ->
    F = fun() -> mnesia:write(#db_mnesia_player{uuid = UUID, data=Data})
    end,
    mnesia:transaction(F).

read_from_db(Key) -> 
    F = fun() -> mnesia:read({db_mnesia_player, Key})
    end,
    mnesia:transaction(F).

encode_message(Data, Packet_name) ->
    Data2 = adamantum_decode:encode_message(Data, Packet_name),
    gen_server:cast(?MODULE, Data2),
    ok.
