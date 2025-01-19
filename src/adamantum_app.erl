%%%-------------------------------------------------------------------
%% @doc adamantum public API
%% @end
%%%-------------------------------------------------------------------

-module(adamantum_app).

-behaviour(application).

-export([setup/0,start/2, stop/1]).

setup() ->    
    ok = mnesia:create_schema([node()]),
    application:start(mnesia),
    adamantum_chunk_manager:setup(),
    adamantum_player_manager:setup().

start(_StartType, _StartArgs) ->
    application:start(mnesia),
    adamantum_chunk_manager:setup(),
    adamantum_player_manager:setup(),
    adamantum_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
