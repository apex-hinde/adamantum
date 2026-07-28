-type uuid() :: bitstring().
-type local_identifier() :: string().
-type text_component() :: map().
-type varint() :: integer().
-type varlong() :: integer().
-type optional(V) :: V | undefined.
-type nbt() :: list().
-type bitset() :: integer().
-type array(V) :: list(V).
-type byte_array() :: binary().
-type properties(A,B,C) :: list({A, B, C}).
-type enum(T) :: T.
-type game_profile() :: {uuid(), string(), properties(string(), string(), optional(string()))}.
-type profile() :: {optional(string()), optional(uuid()), properties(string(), string(), optional(string()))}.
-type type() :: atom().

-record(bool, {
    bool:: boolean()
}).
-record(byte, {
    byte:: integer()
}).
-record(ubyte, {
    ubyte:: integer()
}).
-record(short, {
    short:: integer()
}).
-record(ushort, {
    ushort:: integer()
}).
-record(int, {
    int:: integer()
}).
-record(long, {
    long:: integer()
}).
-record(float, {
    float:: number()
}).
-record(double, {
    double:: number()
}).
-record(string, {
    string:: string()
}).
-record(text_component, {
    component_map:: text_component()
}).
-record(json_text_component, {
    json_component_map:: text_component()
}).
-record(identifier, {
    identifier:: local_identifier()
}).
-record(varint, {
    varint :: varint()
}).
-record(varlong, {
    varlong:: varlong()
}).
-record(entity_metadata, {}). %todo
-record(slot, {
    item_count:: varint()
    , itemID:: optional(varint())
    , components_to_add:: list()
    , components_to_remove:: list()
}).
-record(hashed_slot, {
    item_count:: varint()
    , itemID:: optional(varint())
    , components_to_add:: list()
    , components_to_remove:: list()
}).
-record(nbt, {
    nbt:: nbt()
}).
-record(position, {
    x :: integer()
    , y :: integer()
    , z :: integer()
}).
-record(angle, {
    angle:: number()
}).
-record(uuid, {
    uuid:: uuid()
}).
-record(bitset, {
    bitset:: bitset()
}).
-record(fixed_bitset, {
    fixed_bitset:: bitset()
}).
-record(optional, {
    some:: atom()
    , optional:: optional(any())
}).
-record(prefixed_optional, {
    some:: atom()
    , prefixed_optional:: optional(any())
}).
-record(array, {
    array:: array(any())
}).
-record(prefixed_array, {
    prefixed_array:: array(any())
}).
-record(enum, {
    enum:: enum(any())
}).
-record(byte_array, {
    byte_array:: byte_array()
}).
-record(id_or_x, {
    id_or_x :: any()
}).
-record(id_set, {
    id_set:: array(varint())
}).
-record(sound_event, {
    sound_name:: local_identifier()
    , has_fixed_value:: boolean()
    , fixed_range:: optional(number())
}).

-record(teleport_flags, {
    flagsmap:: map()
}).
-record(light_data, {}). %%todo
-record(either_x_or_y, {
    x:: undefined | any()
    , y:: undefined | any()
}).
-record(game_profile, {
    uuid:: uuid()
    , username:: string()
    , properties:: properties(string(), string(), optional(string()))
}).
-record(resolvable_profile, {
    profile_kind:: enum(varint())
    , profile:: game_profile() | profile() | any()
    , body:: optional(local_identifier()) | any()
    , cape:: optional(local_identifier()) | any()
    , elytra:: optional(local_identifier()) | any()
    , model:: optional(varint()) | any()
}).
-record(debug_subscription_event, {
    debug_subscription_type:: enum(varint())
    , data:: any()
}).
-record(lp_vec3, {
    x :: float()
    , y :: float()
    , z :: float()
}).



%% slot display records
-record(empty, {
    type :: type()
}).
-record(any_fuel, {
    type :: type()
}).
-record(with_any_potion, {
    type :: type()
    , base
}).
-record(only_with_component, {
    type :: type()
    , base
    , component_type_id
}).
-record(item, {
    type :: type()
    , item_type
}).
-record(item_stack, {
    type :: type()
    , item_stack
}).
-record(tag, {
    type :: type()
    , tag
}).
-record(dyed, {
    type :: type()
    , dye
    , target
}).
-record(smithing_trim, {
    type :: type()
    , base
    , material
    , pattern
}).
-record(with_remainder, {
    type :: type()
    , ingredient
    , remainder
}).
-record(composite, {
    type :: type()
    , options_count 
    , options
}).


%% recipe display records
-record(crafting_shapeless, {
    type :: type()
    , ingredients_count
    , ingredients
    , result
    , crafting_station
}).
-record(crafting_shaped, {
    type :: type()
    , width
    , height
    , ingredients_count
    , ingredients
    , result
    , crafting_station
}).
-record(furnace, {
    type :: type()
    , ingredient
    , fuel
    , result
    , crafting_station
    , cooking_time
    , experience
}).
-record(stonecutter, {
    type :: type()
    , ingredient
    , result
    , crafting_station
}).
-record(smithing, {
    type :: type()
    , template
    , base
    , addition
    , result
    , crafting_station
}).

%% debug subscription data records
-record(dedicated_server_tick_time, {
    type :: type()
}).
-record(bee, {
    type :: type()
    , hive_position
    , flower_position
    , travel_ticks
    , blacklisted_hives
}).
-record(villager_brain, {
    type :: type()
    , name
    , profession
    , xp
    , health
    , max_health
    , inventory
    , wants_golem
    , anger_level
    , activities
    , behaviors
    , memories
    , gossips
    , pois
    , potential_pois
}).
-record(breeze, {
    type :: type()
    , attack_target
    , jump_target
}).
-record(goal_selector, {
    type :: type()
    , priority
    , is_running
    , name
}).
-record(entity_path, {
    type :: type()
    , reached
    , next_block_index
    , block_position
    , nodes
    , target_nodes
    , open_set
    , closed_set
    , max_node_distance
}).
-record(entity_block_intersection, {
    type
    , id
}).
-record(bee_hive, {
    type :: type()
    , hive_type
    , occupant_count
    , honey_level
    , sedated
}).
-record(poi, {
    type :: type()
    , position
    , poi_type
    , free_ticket_count
}).
-record(redstone_wire_orientation, {
    type :: type()
    , id
}).
-record(village_section, {
    type :: type()
}).
-record(raid, {
    type :: type()
    , positions
}).
-record(structure, {
    type :: type()
    , structures
}).
-record(game_event_listener, {
    type :: type()
    , listener_radius
}).
-record(neighbor_update, {
    type :: type()
    , position
}).
-record(game_event, {
    type :: type()
    , event
    , x
    , y
    , z
}).
-record(debug_path_node, {
    x
    , y
    , z
    , walk_cost
    , penalty
    , open
    , type
    , heap_index
}).
-record(debug_structure_info, {
    min_x
    , min_y
    , min_z
    , max_x
    , max_y
    , max_z
    , pieces
}).
-record(debug_structure_piece, {
    min_x
    , min_y
    , min_z
    , max_x
    , max_y
    , max_z
    , is_start
}).


%% custom packet records
-record(seen_advancements, {type, action, tab_id}).
-record(boss_bar, {
    type
    , uuid
    , action
    , title
    , health
    , color
    , division
    , flags
}).
-record(node, {
    flags
    , children_count
    , children
    , redirect_node
    , name
    , parser_id
    , properties
    , suggestions_type
}).
-record(delete_chat, {
    message_id
    , signature
}).
-record(chat_type, {
    translation_key
    , parameters
    , style
}).
-record(player_info_update, {
    actions
    , players
}).
-record(player_info_entry, {
    uuid
    , actions
}).
-record(set_equipment, {
    entity_id
    , equipment
}).
-record(set_objective, {
    objective_name
    , mode
    , objective_value
    , type
    , number_format
}).
-record(set_player_team, {
    team_name
    , method
    , team_display_name
    , team_prefix
    , team_suffix
    , name_tag_visibility
    , collision_rule
    , team_color
    , friendly_flags
    , entities
}).
-record(waypoint_data, {
    waypoint_type
    , x
    , y
    , z
    , angle
}).
-record(stop_sound, {
    flags
    , source
    , sound
}).
-record(set_score, {
    entity_name
    , objective_name
    , value
    , display_name
    , number_format
}).

-record(update_advancements, {
    reset
    , advancement_mapping
    , identifiers
    , progress_mapping
}).
-record(advancement, {
    parent_id
    , display_data
    , requirements
    , sends_telemetry
}).
-record(advancement_display, {
    title
    , description
    , icon
    , frame_type
    , flags
    , background_texture
    , x
    , y
}).
-record(advancement_progress, {
    criteria
}).