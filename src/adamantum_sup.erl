%%%-------------------------------------------------------------------
%% @doc adamantum top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(adamantum_sup).

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
        [{adamantum_listen,
            {adamantum_listen, start_link, [adamantum_listen, {25565}]},
            permanent, brutal_kill, worker, [adamantum_listen]},
        {adamantum_chunk_manager,
            {adamantum_chunk_manager, start_link, []},
            permanent, brutal_kill, worker, [adamantum_chunk_manager]},
        {adamntum_player_manager,
            {adamantum_player_manager, start_link, []},
            permanent, brutal_kill, worker, [adamantum_player_manager]},
        {adamntum_uuid,
            {adamantum_uuid, start_link, []},
            permanent, brutal_kill, worker, [adamantum_uuid]}
        ]}}.

%% internal functions
