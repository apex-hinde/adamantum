-module(player).

-behaviour(gen_server).
-include("src/data_types/records.hrl").
-export([start_link/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2, init/1, stop/1, send_message/3, get_name/0]).

-define(SERVER, ?MODULE).
-define(HANDSHAKE, 0).
-define(STATUS, 1).
-define(LOGIN, 2).
-define(TRANSFER, 3).
-define(CONFIGURATION, 4).
-define(PLAY, 5).

-record(state, {listen_pid, listen_socket, player_socket, queue, state_of_play, uuid, keep_alive}).
%% api
get_name() ->
    ?MODULE.
stop(Name) ->
    gen_server:call(Name, stop).

start_link(Listen_pid, Listen_socket) ->

    gen_server:start_link({local, ?SERVER}, ?MODULE, [Listen_pid, Listen_socket], []).

init([Listen_pid, Listen_socket]) ->
    self() ! run_accept,
    {ok, #state{listen_pid = Listen_pid,
            listen_socket = Listen_socket,
            queue = <<>>,
            state_of_play = ?HANDSHAKE}}.

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
    {noreply, NewState}.

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
            io:format("~p : ", [Packet_name]),
            io:format("~p~n", [Decoded]),
            handle_decoded_message({Packet_name, Decoded}, State);
            
        ?STATUS ->
            Packet_name = status:id_to_name(serverbound, Packet_ID),

            Decoded = decode:decode_message(Data2, Packet_name),
            io:format("~p : ", [Packet_name]),
            io:format("~p~n", [Decoded]),
            handle_decoded_message({Packet_name, Decoded}, State);
            
        ?LOGIN ->
            Packet_name = login:id_to_name(serverbound, Packet_ID),

            Decoded = decode:decode_message(Data2, Packet_name),
            io:format("~p : ", [Packet_name]),
            io:format("~p~n", [Decoded]),
            handle_decoded_message({Packet_name, Decoded}, State);
            
        ?CONFIGURATION ->
            Packet_name = configuration:id_to_name(serverbound, Packet_ID),
            io:format("~p : ", [Packet_name]),
            io:format("~p~n", [Data]),
            Decoded = decode:decode_message(Data2, Packet_name),
            handle_decoded_message({Packet_name, Decoded}, State)
%%        ?PLAY ->
%%            Packet_name = play:id_to_name(serverbound, Packet_ID),
%%            io:format("~p : ", [Packet_name]),
%%            io:format("~p~n", [Data]),
%%            Decoded = decode:decode_message(Data2, Packet_name),
%%            handle_decoded_message({Packet_name, Decoded}, State)
    end.



handle_decoded_message(Data, State) ->
    %% login
    {ok, NewState} = decode_messages:decode_message(Data, State),

    NewState.

send_message(PacketName, Record, State) ->
    Encoded = encode_messages:encode_message(PacketName, Record),
    Message = encode:encode_message(Encoded, PacketName, State#state.state_of_play),
    Length = encode:encode_type(byte_size(Message), varint),
    io:format("sent message : ~p~n", [<<Length/binary, Message/binary>>]),
    gen_tcp:send(State#state.player_socket, <<Length/binary, Message/binary>>).









