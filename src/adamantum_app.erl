%%%-------------------------------------------------------------------
%% @doc adamantum public API
%% @end
%%%-------------------------------------------------------------------

-module(adamantum_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    adamantum_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
