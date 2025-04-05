-module(player_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).


-export([start_link/0, 
        stop/0, 
        init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3,
        clear_player_table/0,
        setup/0,
        write_to_db/2,
        read_from_db/1,
        connect_to_PM/2]).
-record(state, {connected_players}).

-record(db_mnesia_player, {uuid, data}).

stop() ->
    gen_server:call(?SERVER, stop).



start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% API
connect_to_PM(UUID, PID) ->
    gen_server:cast(?SERVER, {connect, UUID, PID}).

write_to_db(UUID, Data) ->
    gen_server:cast(?SERVER, {write_to_db, UUID, Data}).

read_from_db(Key) ->
    gen_server:call(?SERVER, {read_from_db, Key}).


init(_Args) ->
    erlang:send_after(50, self(), tick),
    {ok, #state{connected_players = maps:new()}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({read_from_db, Key}, _From, State) ->
    Return = 
        case mnesia:dirty_read(db_mnesia_player, Key) of 
            [] -> 
                [];
            [DB] ->
                DB#db_mnesia_player.data
        end,
    {reply, Return, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
    
handle_cast({connect, UUID, PID}, State) ->
    {noreply, State#state{connected_players = maps:put(UUID, PID, State#state.connected_players)}};


handle_cast({write_to_db, UUID, Data}, State) ->
    F = fun() -> mnesia:write(#db_mnesia_player{uuid = UUID, data=Data})
        end,
    mnesia:transaction(F),
    {noreply, State};

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    erlang:send_after(50, self(), tick),
    send_to_all_players(tick, State),
    {noreply, State};



handle_info(_Info, State) ->
    {noreply, State}.


%db functions
setup() ->
    mnesia:create_table(db_mnesia_player,
                        [{attributes, record_info(fields, db_mnesia_player)},
                         {type, set}, {disc_copies, [node()]}]).

clear_player_table() ->
    mnesia:clear_table(db_mnesia_player).


send_to_all_players(Message, State) ->
    Player_list = maps:to_list(State#state.connected_players),
    lists:foreach(fun({_, PID}) -> player:tick(PID, Message) end, Player_list).
