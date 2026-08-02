-module(player_manager).
-behaviour(gen_server).
-include("src/mnesia_records/player_records.hrl").

%% API
-export([stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([setup/0, clear_player_table/0, insert_player/1, get_player/1,
         check_if_player_exists/1, delete_player/1]).
-record(state, {dummy}).

%% ---------------------------------------------------------------------------
%% Mnesia helpers (mirror world_manager)
%% ---------------------------------------------------------------------------

setup() ->
    case mnesia:create_table(db_player,
                        [{attributes, record_info(fields, db_player)},
                         {type, set}, {disc_copies, [node()]}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, db_player}} -> ok;
        {aborted, _Reason} ->
            mnesia:create_table(db_player,
                        [{attributes, record_info(fields, db_player)},
                         {type, set}, {ram_copies, [node()]}])
    end,
    mnesia:wait_for_tables([db_player], 5000).

clear_player_table() ->
    mnesia:clear_table(db_player).

%% @doc Insert or overwrite a #db_player{} row. Key is uuid.
insert_player(#db_player{uuid = Uuid} = Player) when is_binary(Uuid) ->
    mnesia:dirty_write(Player).

%% @doc Lookup by uuid. Returns {ok, #db_player{}} | {error, not_found}.
get_player(Uuid) when is_binary(Uuid) ->
    case mnesia:dirty_read({db_player, Uuid}) of
        [] -> {error, not_found};
        [Player] -> {ok, Player}
    end.

%% @doc Like world_manager:check_if_chunk_exists/2.
check_if_player_exists(Uuid) when is_binary(Uuid) ->
    case mnesia:dirty_read({db_player, Uuid}) of
        [] -> {false, undefined};
        [Player] -> {true, Player}
    end.

delete_player(Uuid) when is_binary(Uuid) ->
    mnesia:dirty_delete({db_player, Uuid}).

%% ---------------------------------------------------------------------------
%% gen_server
%% ---------------------------------------------------------------------------

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
