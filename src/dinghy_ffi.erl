-module(dinghy_ffi).

-export([start/0, identity/1, start_cluster/4, start_or_restart_cluster/5, start_server/4,
    process_command/3, members_from_name/2, members/2, members_local/2, add_member/3,
    server_id_node/1, query_leader/3, query_replica/3, leave_and_terminate/3,
    trigger_election/2, delete_cluster/2]).

start() -> ra:start().

identity(Val) -> Val.

start_cluster(ClusterName, Function, InitialState, ServerIds) ->
    case ra:start_cluster(default, ClusterName, {simple, Function, InitialState}, ServerIds) of
        {ok, Started, NotStarted} -> {ok, {cluster_start_result, Started, NotStarted}};
        {error, cluster_not_formed} -> {error, nil}
    end.

start_or_restart_cluster(ClusterName, Function, InitialState, ServerIds, Timeout) ->
    case ra:start_or_restart_cluster(default, ClusterName, {simple, Function, InitialState}, ServerIds, Timeout) of
        {ok, Started, NotStarted} -> {ok, {cluster_start_result, Started, NotStarted}};
        {error, cluster_not_formed} -> {error, nil}
    end.

start_server(ClusterName, Function, InitialState, ServerIds) ->
    case ra:start_server(default, ClusterName, {simple, Function, InitialState}, ServerIds) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.

process_command(ServerId, Command, Options) ->
    case ra:process_command(ServerId, Command, Options) of
        {ok, Reply, Leader} -> {ok, {Reply, Leader}};
        {timeout, _ServerId} -> {error, timeout};
        {error, Error} -> {error, {other, Error}}
    end.

members_from_name(ClusterName, Timeout) ->
    members(ClusterName, Timeout).

members(Servers, Timeout) ->
    case ra:members(Servers, Timeout) of
        {ok, Members, LeaderId} -> {ok, {Members, LeaderId}};
        {timeout, _ServerId} -> {error, timeout};
        {error, Error} -> {error, {other, Error}}
    end.

members_local(Server, Timeout) ->
    members({local, Server}, Timeout).

add_member(Existing, New, Timeout) ->
    case ra:add_member(Existing, New, Timeout) of
        {ok, _, _} -> {ok, nil};
        {error, already_member} -> {error, already_member};
        {error, cluster_change_not_permitted} -> {error, not_allowed}
    end.

server_id_node(ServerId) ->
    {_, Result} = ServerId,
    ServerId.

query_leader(Server, Fun, Timeout) ->
    case ra:leader_query(Server, Fun, Timeout) of
        {ok, {_TermMeta, Result}, LeaderId} -> {ok, {query_result, Result, LeaderId}};
        {timeout, _ServerId} -> {error, timeout};
        {error, Error} -> {error, {other, Error}}
    end.

query_replica(Server, Fun, Timeout) ->
    case ra:local_query(Server, Fun, Timeout) of
        {ok, {_TermMeta, Result}, LeaderId} -> {ok, {query_result, Result, LeaderId}};
        {timeout, _ServerId} -> {error, timeout};
        {error, Error} -> {error, {other, Error}}
    end.

leave_and_terminate(Server, ToRemove, Timeout) ->
    case ra:leave_and_terminate(default, Server, ToRemove, Timeout) of
        ok -> {ok, nil};
        timeout -> {error, timeout};
        {error, Error} -> {error, {other, Error}}
    end.

trigger_election(Server, Timeout) ->
    ra:trigger_election(Server, Timeout),
    nil.

delete_cluster(Server, Timeout) ->
    case ra:delete_cluster([Server], Timeout) of
        {ok, Leader} -> {ok, Leader};
        {error, Reason} -> {error, Reason}
    end.
