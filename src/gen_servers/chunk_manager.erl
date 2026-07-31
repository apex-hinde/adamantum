-module(chunk_manager).
-behaviour(gen_server).
-include("src/mnesia_records/world_records.hrl").
%% API
-export([stop/1, start_link/1, get_chunk/3, get_block/4, set_block/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {chunks}).
%% API

stop(PID) ->
    gen_server:call(PID, stop).

start_link({BottomLeftX, BottomLeftZ}) ->
    gen_server:start_link(?MODULE, [BottomLeftX, BottomLeftZ], []).

get_chunk(PID, ChunkX, ChunkZ) ->
    gen_server:call(PID, {get_chunk, ChunkX, ChunkZ}).

get_block(PID, BlockX, BlockY, BlockZ) ->
    gen_server:call(PID, {get_block, BlockX, BlockY, BlockZ}).

set_block(PID, BlockX, BlockY, BlockZ, BlockId) ->
    gen_server:call(PID, {set_block, BlockX, BlockY, BlockZ, BlockId}).

init([BottomLeftX, BottomLeftZ]) ->
    StartChunkX = BottomLeftX * 16,
    StartChunkZ = BottomLeftZ * 16,

    ChunksMap = generate_initial_chunks(StartChunkX, StartChunkZ),
    {ok, #state{chunks=ChunksMap}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({get_chunk, ChunkX, ChunkZ}, _From, State) ->
    Reply = case State#state.chunks of
       #{{ChunkX, ChunkZ} := ChunkColumn} ->
           {ok, ChunkColumn};
       #{} ->
           {error, not_found}
   end,
    {reply, Reply, State};

handle_call({get_block, BlockX, BlockY, BlockZ}, _From, State) ->
    ChunkX = BlockX bsr 4,
    ChunkZ = BlockZ bsr 4,
    Reply = case maps:find({ChunkX, ChunkZ}, State#state.chunks) of
        {ok, ChunkColumn} ->
            LocalX = BlockX band 15,
            LocalY = BlockY band 15,
            LocalZ = BlockZ band 15,
            Section = chunk:get_chunk_from_chunk_column(ChunkColumn, BlockY),
            BlockId = chunk:get_block_at_index(Section, LocalX, LocalY, LocalZ),
            {ok, BlockId};
        error ->
            {error, not_found}
    end,
    {reply, Reply, State};

handle_call({set_block, BlockX, BlockY, BlockZ, BlockId}, _From, State) ->
    ChunkX = BlockX bsr 4,
    ChunkZ = BlockZ bsr 4,
    case maps:find({ChunkX, ChunkZ}, State#state.chunks) of
        {ok, ChunkColumn} ->
            LocalX = BlockX band 15,
            LocalY = BlockY band 15,
            LocalZ = BlockZ band 15,
            Section = chunk:get_chunk_from_chunk_column(ChunkColumn, BlockY),
            NewSection = chunk:set_block_at_index(LocalX, LocalY, LocalZ, BlockId, Section),
            NewChunkColumn = chunk:insert_chunk_into_chunk_column(ChunkColumn, BlockY, NewSection),
            NewChunks = maps:put({ChunkX, ChunkZ}, NewChunkColumn, State#state.chunks),
            {reply, {ok, BlockId}, State#state{chunks = NewChunks}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


generate_initial_chunks(BottomLeftX, BottomLeftZ) ->
    Keys = [{X, Z} || X <- lists:seq(BottomLeftX, BottomLeftX+15), Z <- lists:seq(BottomLeftZ, BottomLeftZ+15)],
    InitialMap = maps:new(),
    lists:foldr(fun({X,Z}, Acc) ->
                    case world_manager:check_if_chunk_exists(X, Z) of
                        {false, undefined} ->
                            Acc#{{X,Z} => generate_basic_chunk(X, Z)};
                        {true, #db_chunks{chunk_column = ChunkColumn}} ->
                            Acc#{{X,Z} => ChunkColumn}
                    end
                end, InitialMap, Keys).

    


global_xz_to_chunk_xz(X,Z) ->
    ChunkX = (X) bsr 4,
    ChunkZ = (Z) bsr 4,
    {ChunkX, ChunkZ}.
    
    
generate_basic_chunk(X, Z) ->
    chunk:normal_chunk_column(X, Z).



