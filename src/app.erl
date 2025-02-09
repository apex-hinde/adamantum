%%%-------------------------------------------------------------------
%% @doc mylib public API
%% @end
%%%-------------------------------------------------------------------

-module(app).

-behaviour(application).

-export([start/2, stop/1, setup/0]).

setup() ->
    mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]).

start(_StartType, _StartArgs) ->

    inets:start(),
    ssl:start(),
    mnesia:start(),
%    chunk_manager:setup(),
%    chunk_manager:clear_chunk_table(),
    player_manager:setup(),
    player_manager:clear_player_table(),
    sup:start_link().

stop(_State) ->
    ok.

%% internal functions
