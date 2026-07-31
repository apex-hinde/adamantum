-module(world_manager).
-behaviour(gen_server).
-include("src/mnesia_records/world_records.hrl").
%% API
-export([stop/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([setup/0, clear_chunk_table/0, check_if_chunk_exists/2, get_chunk_managers/2, get_chunk_manager/2]).
-record(state, {db_id, chunk_manager_map}).
-define(SERVER, ?MODULE).

%% API
check_if_chunk_exists(X, Z) ->
    case mnesia:dirty_read({db_chunks, {X, Z}}) of
        [] -> {false, undefined};
        [Chunk] -> {true, Chunk}
    end.

get_chunk_managers(X,Z) ->
    gen_server:call(?SERVER, {get_chunk_managers, X, Z}, 30000).

get_chunk_manager(ChunkX, ChunkZ) ->
    gen_server:call(?SERVER, {get_chunk_manager, ChunkX, ChunkZ}, 30000).


stop() ->
    gen_server:call(?SERVER, stop).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{db_id=db_chunks, chunk_manager_map=maps:new()}}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({get_chunk_manager, ChunkX, ChunkZ}, _From, State) ->
    Key = chunk_xz_to_chunk_manager_xz(ChunkX, ChunkZ),
    case maps:find(Key, State#state.chunk_manager_map) of
        {ok, PID} ->
            {reply, PID, State};
        error ->
            {ok, NewPID} = chunk_manager:start_link(Key),
            NewMap = maps:put(Key, NewPID, State#state.chunk_manager_map),
            {reply, NewPID, State#state{chunk_manager_map = NewMap}}
    end;

handle_call({get_chunk_managers, X, Z}, _From, State) ->
    {CenterMX, CenterMZ} = global_xz_to_chunk_manager_xz(X, Z),
    Keys = [{MX, MZ} || MX <- lists:seq(CenterMX - 1, CenterMX + 1),
                        MZ <- lists:seq(CenterMZ - 1, CenterMZ + 1)],
    {Pids, NewMap} = lists:foldl(
        fun(Key, {AccPids, AccMap}) ->
            case maps:find(Key, AccMap) of
                {ok, PID} ->
                    {[PID | AccPids], AccMap};
                error ->
                    {ok, NewPID} = chunk_manager:start_link(Key),
                    {[NewPID | AccPids], maps:put(Key, NewPID, AccMap)}
            end
        end,
        {[], State#state.chunk_manager_map},
        Keys
    ),
    {reply, lists:reverse(Pids), State#state{chunk_manager_map = NewMap}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.



handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

setup() ->
    case mnesia:create_table(db_chunks,
                        [{attributes, record_info(fields, db_chunks)},
                         {type, set}, {disc_copies, [node()]}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, db_chunks}} -> ok;
        {aborted, _Reason} ->
            mnesia:create_table(db_chunks,
                        [{attributes, record_info(fields, db_chunks)},
                         {type, set}, {ram_copies, [node()]}])
    end,
    mnesia:wait_for_tables([db_chunks], 5000).
clear_chunk_table() ->
    mnesia:clear_table(db_chunks).




global_xz_to_chunk_xz(X,Z) ->
    ChunkX = (X) bsr 4,
    ChunkZ = (Z) bsr 4,
    {ChunkX, ChunkZ}.
global_xz_to_chunk_manager_xz(X,Z) ->
    ChunkX = (X) bsr 8,
    ChunkZ = (Z) bsr 8,
    {ChunkX, ChunkZ}.
chunk_xz_to_chunk_manager_xz(ChunkX, ChunkZ) ->
    MX = ChunkX bsr 4,
    MZ = ChunkZ bsr 4,
    {MX, MZ}.
    


