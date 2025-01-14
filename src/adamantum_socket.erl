%need to fix UUID with api request currently just setting UUID as mine
%need to set up compression currently disables
%set eid to depend on database number



-module(adamantum_socket).
-behaviour(gen_server).
%-include

%% API
-export([stop/1, start_link/2, messages/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, 
        {
            socket,
            listen_pid,
            listen_socket,
            state_of_connection = handshake,
            username,
            uuid,
            eid
        }).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Listen_pid, Listen_socket) ->
    gen_server:start_link(?MODULE, [Listen_pid, Listen_socket], []).

init([Listen_pid, Listen_socket]) ->
    self() ! run_accept,
    {ok, #state{listen_pid=Listen_pid,
        listen_socket=Listen_socket}}.



handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(run_accept, State) ->
    {ok, Socket} = gen_tcp:accept(State#state.listen_socket),
    gen_server:cast(State#state.listen_pid, connected),
    NewState = State#state{socket = Socket},
    {noreply, NewState};

handle_info({tcp, Socket, Data}, State) ->
    io:format("~p~n", [Data]),
    NewState = messages(Data, State),
    {noreply, NewState};

handle_info({tcp_closed,_}, State) ->
    {stop, normal ,State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



messages(Data, State) ->
    {Len,Tail} = varint:decode_varint(Data),
    <<Packet_id:1/binary,Content/binary>> =  Tail,
    Size = byte_size(Tail),
    if Len =:= Size ->
        case State#state.state_of_connection of
            handshake ->
                case Packet_id of
                    <<0>> ->
                        Next_state = adamantum_decode:decode_handshake(Content),
                        NewState = State#state{state_of_connection = Next_state},
                        NewState;
                    _ ->
                        io:format("~p~n",[["error in handshake"+ Data]]),
                        State
                    end;
            login ->
                NewState = login(Content, Len, Packet_id, State),
            NewState;
            play ->
                State
            
        end;
    true ->
        State
    end.

    


login(Data, Len, Packet_id, State) ->

        UUID = <<"c534f6a0-7882-3f79-8143-0107ae25aba5">>,
        Eid = <<0,0,1,74>>,
        {Len_of_username, Username} = varint:decode_varint(Data),
        Message_login_success = adamantum_encode:login_success(Username, UUID, Len_of_username),
%        Message_set_compression = adamantum_encode:set_compression(),
        Message_join_game = adamantum_encode:join_game(Eid),
        Message_spawn_position = adamantum_encode:spawn_position(0,0,100),
        Message_player_position_and_look = adamantum_encode:player_position_and_look(0,100,0,0,0,0),

        send_message(Message_login_success, State),
%            send_message(Message_set_compression, State),
        send_message(Message_join_game, State),
        io:fwrite("this is me~p~n", [Message_spawn_position]),
        send_message(Message_spawn_position, State),
        send_message(Message_player_position_and_look, State),
%        Message_chunk_data = adamantum_encode:map_chunk_bulk(0,0),
%        send_message(Message_chunk_data, State),


        
        NewState = State#state{username = Data,
                                uuid = UUID,
                                state_of_connection = play,
                                eid = Eid},
                                
        NewState.
        
play(Data, Len, Packet_id, State) ->
    case Packet_id of 
        <<0>> ->
            send_message(<<2,0,0>>, State)
    end.


send_message(Data, State)->

    gen_tcp:send(State#state.socket, Data).








