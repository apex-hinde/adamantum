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
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => adamantum,
                    start => {adamantum_listen, start_link, [adamantum_listen, 25565]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [adamantum_listen]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
