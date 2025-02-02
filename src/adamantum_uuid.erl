-module(adamantum_uuid).
-behaviour(gen_server).
-export([init/1, 
        start_link/0, 
        stop/0, 
        handle_call/3, 
        handle_cast/2,
        get_uuid/1]).
-define(SERVER, ?MODULE).
-record(state,{dummy}).

stop() ->
    gen_server:call(?SERVER, stop).



start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% api
get_uuid(Username) ->
    gen_server:call(?SERVER, {get_uuid, Username}).

init(_Args) ->
    {ok, #state{dummy = 1}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({get_uuid, Username}, _From, State) ->

    Request = string:concat("https://api.mojang.com/users/profiles/minecraft/", Username),
    case httpc:request(get, {Request, []}, [], []) of   
        {error, Reason} -> 
            io:format("Error: ~p~n", [Reason]),
            {reply, ok, State};
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            Json = json:decode(list_to_binary(Body)),
            Uuid = maps:get(<<"id">>, Json),
            {reply, trim_to_full(binary_to_list(Uuid)), State}
    end;

    
    

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


trim_to_full(UUID) ->



string:join([string:substr(UUID, 1, 8), "-", string:substr(UUID, 9, 4), "-", string:substr(UUID, 13, 4), "-", string:substr(UUID, 17, 4), "-", string:substr(UUID, 21, 12)], "").

    
