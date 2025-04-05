-module(configuration).
-export([get_configuration/1]).
-define(VERSION, 769).
-define(RENDER_DISTANCE, 8).

get_configuration(Type) ->
    case Type of
        version ->
            ?VERSION;
        render_distance ->
             ?RENDER_DISTANCE
    end.