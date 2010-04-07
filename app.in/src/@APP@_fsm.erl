%%%----------------------------------------------------------------------
%%%
%%% @copyright @COPYRIGHT@
%%%
%%% @author @AUTHOR@ <@MAIL@>
%%% @doc an fsm demo server
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(@APP@_fsm).
-author("@MAIL@").
-vsn('0.1').
-behaviour(gen_fsm).
-include("@APP@.hrl").

-export([start_link/0]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
                            terminate/3, code_change/4]).
-export([demo_state/2, demo_state/3]).

%% @doc start the server
-spec start_link() -> {'ok', any()} | 'ignore' | {'error', any()}.
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% gen_fsm callbacks
%%
init(_Args) ->    
    {ok, demo_state, []}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, handled, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_Old, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

demo_state(_Event, _From, State) ->
    {next_state, demo_state, State}.

demo_state(_Event, State) ->
    {reply, handled, demo_state, State}.
    
%%
%% internal API
%%
