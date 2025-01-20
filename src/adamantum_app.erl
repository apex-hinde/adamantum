%%%-------------------------------------------------------------------
%% @doc adamantum public API
%% @end
%%%-------------------------------------------------------------------

-module(adamantum_app).

-behaviour(application).

-export([setup_chunks/0,start/2, stop/1, setup_player/0]).

setup_chunks() ->    
    ok = mnesia:create_schema([node()]),
    application:start(mnesia),
    adamantum_chunk_manager:setup().

setup_player() ->
    adamantum_player_manager:setup().


start(_StartType, _StartArgs) ->
    application:start(mnesia),
    adamantum_chunk_manager:setup(),
    adamantum_player_manager:setup(),
    adamantum_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
