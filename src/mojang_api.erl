-module(mojang_api).
-export([get_profile/1,
        get_player_uuid/1]).

get_profile(UUID) ->

    Request = string:concat("https://sessionserver.mojang.com/session/minecraft/profile/", UUID),

    case httpc:request(get, {Request, []}, [], []) of   
        {error, Reason} -> 
            io:format("Error: ~p~n", [Reason]),
            {error, Reason};


        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            Json = json:decode(list_to_binary(Body)),
            [Properties] = maps:get(<<"properties">>, Json),
            Name = maps:get(<<"name">>, Properties),
            Value = maps:get(<<"value">>, Properties),
            {Name, Value}
    end.

get_player_uuid(Username) ->
    Request = string:concat("https://api.mojang.com/users/profiles/minecraft/", Username),
    case httpc:request(get, {Request, []}, [], []) of   
        {error, Reason} -> 
            io:format("Error: ~p~n", [Reason]);

        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            Json = json:decode(list_to_binary(Body)),
            maps:get(<<"id">>, Json)
    end.