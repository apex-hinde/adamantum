-module(adamantum_player_manager).
-behaviour(gen_server).
-export([stop/1,start_link/2,init/1,terminate/2,code_change/3,setup/0, handle_info/2]).
-export([handle_call/3,handle_cast/2]).
-record(state, 
        {listen_pid, listen_socket, state_of_play = handshake, socket}).
-record(db_mnesia_player, {uuid, data}).


setup() ->
    mnesia:create_table(db_mnesia_player, [{attributes, record_info(fields, db_mnesia_player)},
                                 {type, set}, {disc_copies, [node()]}]).

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
    {noreply, State}.

handle_info(run_accept, State) ->
    {ok, Socket} = gen_tcp:accept(State#state.listen_socket),
    gen_server:cast(State#state.listen_pid, connected),
    NewState = State#state{socket = Socket},
    {noreply, NewState};


handle_info({tcp, _Socket, Data}, State) ->
    io:format("~p~n",[Data]),
    {Length, Data2} = varint:decode_varint(Data),
    {PacketID, Data3} = varint:decode_varint(Data2),
    case State#state.state_of_play of
        handshake ->
            NewState = handshake(Data3),
            if NewState == status -> 
                io:format("~p~n", ["status"]);
            true ->
                io:format("~p~n", ["login"])
            end,
            {noreply, State#state{state_of_play=NewState}};
        login ->
            login(Data3, Length, State),
            {noreply, State#state{state_of_play=play}};
        play ->
            io:format("~p~n", [Data]),
            Result = adamantum_decode:decode_message(Data3, PacketID),
            {noreply, State}
    end.
    

login(Data, _Len, State) ->
    UUID = <<"c534f6a0-7882-3f79-8143-0107ae25aba5">>,
    Eid = <<0,0,1,74>>,
    {Len_of_username, Username} = varint:decode_varint(Data),
    Message_login_success = adamantum_encode:login_success(Username, UUID, Len_of_username),
    Message_set_compression = adamantum_encode:set_compression(),
    Message_join_game = adamantum_encode:join_game(Eid),
    Message_spawn_position = adamantum_encode:spawn_position(0,0,100),
    Message_player_position_and_look = adamantum_encode:player_position_and_look(0,100,0,0,0,0),
%    Message_chunk_data = adamantum_encode:chunk_data({0,0}),
%    send_message(Message_set_compression, State),
    send_message(Message_login_success, State),
    send_message(Message_join_game, State),
    send_message(Message_spawn_position, State),
    send_message(Message_player_position_and_look, State),
%    send_message(Message_chunk_data, State),


    
%        NewState = State#state{username = Data,
%                                uuid = UUID,
%                                state_of_connection = play,
%                                eid = Eid},
                                
        State.
handshake(Data) ->
    Next_state = binary:last(Data),
    case Next_state of
        1 ->
            State = status,
            State;
        2 ->
            State = login,
            State
    end.




send_message(Message, State) ->
    Length = varint:encode_varint(byte_size(Message)),

    gen_tcp:send(State#state.socket, <<Length/binary, Message/binary>>).