-module(adamantum_listen).
-behaviour(gen_server).

%% API
-export([stop/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {ls}).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name, Port) ->
    gen_server:start_link({local, Name}, ?MODULE, [Port], []).

init([Port]) ->
    {ok, Socket} = gen_tcp:listen(Port, [binary, {active, true}, {reuseaddr, true}]),
    adamantum_socket:start_link(self(), Socket), 
    {ok, #state{ls=Socket}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    adamantum_socket:start_link(self(), State#state.ls), 
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
