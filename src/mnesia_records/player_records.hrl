-include("src/data_types/player_records.hrl").

%% First field is the mnesia key (player UUID binary).
-record(db_player, {
    uuid :: binary(),
    inventory :: #inventories{},
    position :: #player_position{}
}).
