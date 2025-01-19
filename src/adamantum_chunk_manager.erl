-module(adamantum_chunk_manager).
-behaviour(gen_server).

%% API
-export([stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, setup/0, get_chunk_column/1, clear_chunk_table/0]).
-record(state, {dummy}).
-record(db_mnesia_chunk, {coords, data}).


stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(get_chunk, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


clear_chunk_table() ->
    mnesia:clear_table(db_mnesia_chunk).
setup() ->
    mnesia:create_table(db_mnesia_chunk, [{attributes, record_info(fields, db_mnesia_chunk)},
                                 {type, set}, {disc_copies, [node()]}]).


get_chunk_column({X,Y}) ->
    X_chunk = floor(X / 16),
    Y_chunk = floor(Y / 16),
    {atomic, Result} = read_chunk({X_chunk, Y_chunk}),
    case Result of
        [] -> 
            Chunk = adamantum_chunk_generator:gen_column(),
            write_chunk({X_chunk, Y_chunk}, Chunk),
            Chunk;
        [Chunk] -> Chunk,
            Chunk#db_mnesia_chunk.data
    end.
    

read_chunk({X,Y}) ->
    F = fun() -> mnesia:read(db_mnesia_chunk, {X,Y}) end,
    mnesia:transaction(F).

write_chunk(Coords, Chunk_data) ->
    F = fun() -> mnesia:write(#db_mnesia_chunk{coords=Coords, data=Chunk_data})
        end,
    mnesia:transaction(F).