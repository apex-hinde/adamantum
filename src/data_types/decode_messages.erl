-module(decode_messages).
-include("src/data_types/records.hrl").
-export([decode_message/2]).

-define(SERVER, ?MODULE).
-define(HANDSHAKE, 0).
-define(STATUS, 1).
-define(LOGIN, 2).
-define(TRANSFER, 3).
-define(CONFIGURATION, 4).
-define(PLAY, 5).
-record(state, {listen_pid, listen_socket, player_socket, queue, state_of_play, uuid, keep_alive}).

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
        _ ->
            io:format("Error: Unknown message type: ~p~n", [Data]),
            ok
    end.

minecraft_intetion(Record, State) ->
    {ok, State#state{state_of_play = Record#'minecraft:intention'.next_state}}.

minecraft_status_request(State) ->
    {ok, Bin} = file:read_file("src/configuration/status_response.json"),
 
    Response = #'minecraft:status_response'{
        json_response = Bin
        },
    player:send_message('minecraft:status_response', Response, State),
    {ok, State#state{state_of_play = ?LOGIN}}.

minecraft_ping_request(Record, State) ->

    Response = #'minecraft:pong_response'{timestamp = Record#'minecraft:ping_request'.timestamp},
    player:send_message('minecraft:pong_response', Response, State),
    {ok, State}.
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
    player:send_message('minecraft:login_finished', Response, State),

    {ok, State#state{uuid = Record#'minecraft:hello_serverbound'.uuid}}.
minecraft_login_acknowledged(_Record, State) ->
    {ok, State#state{state_of_play = ?CONFIGURATION}}.

minecraft_custom_payload(Record, State) ->
    io:format("Received custom_payload: ~p~n", [Record]),
    {ok, State}.

minecraft_client_information(Record, State) ->
    io:format("Received client_information: ~p~n", [Record]),
    {ok, State}.






%%-record('minecraft:hello', {
%%    name:: string()
%%    , uuid :: string()
%%}).
%%-record('minecraft:key', {
%%    public_key :: prefixed_array(byte())
%%    , nonce :: prefixed_array(byte())
%%}).
%%
%%-record('minecraft:login_acknowledged', {}).
%%-record('minecraft:cookie_response', {
%%    cookie :: identifier()
%%    , data :: optional(prefixed_array(byte()))
%%}).



