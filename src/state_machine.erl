-module(dinghy_state_machine).
-behaviour(ra_machine).

-export([init/1, state_enter/2, apply/3, start/4, create_handler_function/1]).

init({State, HandlerFn, StateEnter}) -> {State, HandlerFn, StateEnter}.

state_enter(RaState, {State, _HandlerFn, StateEnter}) ->
    StateEnter(RaState, State).

apply(_Meta, {user_operation, Operation, QueryFunction}, {State, Handler, _StateEnter}) ->
    {NewState, Effects} = HandlerFn(Operation, State),
    Result = QueryFunction(NewState),
    {{NewState, Handler}, Result, Effects}.

apply(_Meta, Operation, {State, Handler, _StateEnter}) ->
    {NewState, Effects} = HandlerFn(Operation, State),
    {{NewState, Handler}, nil, Effects}.

start(Name, State, HandlerFn, StateEnter, Members) ->
    ra:start_cluster(default, Name, {module, ?MODULE, {State, HandlerFn, StateEnter}}, Members).

create_handler_function(Handler) ->
    fun({down, Pid}, State) ->
        Handler({process_down, Pid}, State);
       ({down, Pid, _}, State) ->
        Handler({process_down, Pid}, State);
       ({nodeup, Node}, State) ->
        Handler({node_up, Node}, State);
       ({nodedown, Node}, State) ->
        Handler({node_down, Node}, State);
       (timeout, State) ->
        Handler(timeout, State);
       (Operation, State) ->
        Handler(Operation, State)
    end.
