-module(player_chunk_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/data_types/chunk_records.hrl").

player_chunk_management_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SetupData) ->
         {timeout, 30,
          [
           {"Get surrounding chunk managers", fun test_get_chunk_managers/0},
           {"Get specific chunk manager", fun test_get_chunk_manager/0},
           {"Query chunk column", fun test_query_chunk/0},
           {"Get and set block state", fun test_get_and_set_block/0}
          ]}
     end}.

setup() ->
    application:start(mnesia),
    world_manager:setup(),
    {ok, WorldPid} = world_manager:start_link(),
    WorldPid.

cleanup(_WorldPid) ->
    try world_manager:stop() catch _:_ -> ok end,
    application:stop(mnesia).

test_get_chunk_managers() ->
    %% Player at global block position (0, 0)
    Managers = player:get_surrounding_chunk_managers(0, 0),
    ?assertEqual(9, length(Managers)),
    lists:foreach(fun(Pid) -> ?assert(is_pid(Pid) andalso is_process_alive(Pid)) end, Managers).

test_query_chunk() ->
    [ManagerPid | _] = player:get_surrounding_chunk_managers(0, 0),
    %% The first manager in the 3x3 surrounding list is {-1, -1}, which owns chunk (-16, -16)
    {ok, ChunkColumn} = player:query_chunk(ManagerPid, -16, -16),
    ?assertMatch(#chunk_column{}, ChunkColumn).

test_get_and_set_block() ->
    [ManagerPid | _] = player:get_surrounding_chunk_managers(0, 0),
    %% Block (-16, 64, -16) is owned by chunk manager {-1, -1}
    BlockX = -16,
    BlockY = 64,
    BlockZ = -16,
    
    %% Query block before modification
    {ok, InitialBlock} = player:query_block(ManagerPid, BlockX, BlockY, BlockZ),
    
    %% Modify block to Stone (ID = 1)
    StoneId = 1,
    {ok, StoneId} = player:modify_block(ManagerPid, BlockX, BlockY, BlockZ, StoneId),
    
    %% Query block after modification to verify update
    {ok, UpdatedBlock} = player:query_block(ManagerPid, BlockX, BlockY, BlockZ),
    ?assertEqual(StoneId, UpdatedBlock),
    
    %% Verify adjacent block at (-16, 64, -15) remains unchanged
    {ok, AdjacentBlock} = player:query_block(ManagerPid, BlockX, 64, -15),
    ?assertEqual(InitialBlock, AdjacentBlock).

test_get_chunk_manager() ->
    Pid1 = world_manager:get_chunk_manager(0, 0),
    ?assert(is_pid(Pid1) andalso is_process_alive(Pid1)),
    %% Same chunk manager for chunk (15, 15)
    Pid2 = world_manager:get_chunk_manager(15, 15),
    ?assertEqual(Pid1, Pid2),
    %% Different chunk manager for chunk (16, 0)
    Pid3 = world_manager:get_chunk_manager(16, 0),
    ?assert(is_pid(Pid3) andalso is_process_alive(Pid3)),
    ?assertNotEqual(Pid1, Pid3).

