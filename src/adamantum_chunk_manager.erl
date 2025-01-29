-module(adamantum_chunk_manager).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([stop/0, start_link/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3,
         update_player_chunks/2, setup/0, get_chunk_column/1, clear_chunk_table/0,
         make_player_surround_list/3, chunks_to_add/2, chunks_to_remove/2]).

-record(state, {dummy}).
-record(db_mnesia_chunk, {coords, data}).

stop() ->
    gen_server:call(?SERVER, stop).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

update_player_chunks(Coords, Loaded_chunks) ->
    gen_server:call(?SERVER, {update_player_chunks, Coords, Loaded_chunks}).


init(_Args) ->
    {ok, #state{dummy = 1}}.

handle_call({update_player_chunks, Coords, Loaded_chunks}, _From, State) ->
    {X,Y,Z,Yaw,Pitch} = Coords,
    X_chunk = floor(X / 16),
    Y_chunk = floor(Z / 16),
    Player_chunks_load =
        lists:reverse(make_player_surround_list({X_chunk, Y_chunk},
                                                {X_chunk - 7, Y_chunk - 7},
                                                [])),
    case Loaded_chunks =:= Player_chunks_load of
        true ->
            {reply, no_chunk, State};
        false ->
            Merged_list = lists:merge(Loaded_chunks, Player_chunks_load),
            Chunks_to_remove = lists:subtract(Loaded_chunks, Merged_list),
            Chunks_to_send = lists:subtract(Player_chunks_load, Loaded_chunks),
            io:format("loaded chunks ~p~n", [Loaded_chunks]),

            io:format("chunks surrounding player ~p~n", [Player_chunks_load]),
            io:format("chunks to remove ~p~n", [Chunks_to_remove]),
            io:format("chunks to add ~p~n", [Chunks_to_send]),
            io:format("x,y:  ~p~n", [[X_chunk, Y_chunk]]),



            {reply, {update_player_chunks, Chunks_to_remove, Chunks_to_send, Player_chunks_load}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


chunks_to_remove([], Acc) ->
    Acc;
chunks_to_remove(List_of_chunks, Acc) ->
    [Chunk_to_remove | T] = List_of_chunks,
    chunks_to_remove(T, [{remove, Chunk_to_remove, get_empty_chunk_column()} | Acc]).

chunks_to_add([], Acc) ->
    Acc;
chunks_to_add(List_of_chunks, Acc) ->
    [Chunk_to_remove | T] = List_of_chunks,
    chunks_to_remove(T, [{add, Chunk_to_remove, get_chunk_column(Chunk_to_remove)} | Acc]).

%returns a list which contains all of the chunk coords that should be loaded.
% adamantum_chunk_manager:make_player_surround_list({0,0}, {-8,-8}, []).

    
make_player_surround_list({X, Y}, {X_current, Y_current}, Acc) ->
    Length = length(Acc),
    case Length rem 15 of
        0 ->
            case Length of
                0 ->
                    make_player_surround_list({X, Y},
                                      {X_current + 1, Y_current},
                                      [{X_current, Y_current} | Acc]);
                15 * 15 ->

                    [{X_current, Y_current} | Acc];
                _ ->
                    make_player_surround_list({X, Y},
                                              {X - 8, Y_current + 1},
                                              [{X_current, Y_current} | Acc])
            end;
        _ ->
            make_player_surround_list({X, Y},
                                      {X_current + 1, Y_current},
                                      [{X_current, Y_current} | Acc])
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

clear_chunk_table() ->
    mnesia:clear_table(db_mnesia_chunk).

setup() ->
    mnesia:create_table(db_mnesia_chunk,
                        [{attributes, record_info(fields, db_mnesia_chunk)},
                         {type, set},
                         {disc_copies, [node()]}]).

get_empty_chunk_column() ->
    <<0:256>>.

get_chunk_column({X_chunk, Y_chunk}) ->
%    X_chunk = floor(X / 16),
%    Y_chunk = floor(Y / 16),

    case read_from_db({X_chunk, Y_chunk}) of
        [] ->
            Chunk = adamantum_chunk_generator:gen_column(),
            write_chunk({X_chunk, Y_chunk}, Chunk),
            Chunk;
        Result ->
                Result
    end.

read_from_db({X,Y}) -> 
    case mnesia:dirty_read({db_mnesia_chunk, {X,Y}}) of 
        [] -> 
            [];
        [DB] ->
            DB#db_mnesia_chunk.data
    end.

write_chunk(Coords, Chunk_data) ->
    F = fun() -> mnesia:write(#db_mnesia_chunk{coords = Coords, data = Chunk_data}) end,
    mnesia:transaction(F).

