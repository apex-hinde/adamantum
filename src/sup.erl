%%%-------------------------------------------------------------------
%% @doc mylib top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    {ok, {{one_for_all, 0, 1},
    [{listen,
        {listen, start_link, [listen, {25565}]},
        permanent, brutal_kill, worker, [listen]},
        {player_manager,
        {player_manager, start_link, []},
        permanent, brutal_kill, worker, [player_manager]}]}}.

%% internal functions
