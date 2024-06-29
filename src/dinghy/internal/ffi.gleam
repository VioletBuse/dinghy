import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/node.{type Node}

pub type ClusterName

pub type ServerId

@external(erlang, "dinghy_ffi", "identity")
fn tuple_to_serverid(in: #(ClusterName, Node)) -> ServerId

pub fn server_id(cluster: ClusterName, node: Node) -> ServerId {
  tuple_to_serverid(#(cluster, node))
}

@external(erlang, "dinghy_ffi", "identity")
pub fn cluster_name(name: Atom) -> ClusterName

@external(erlang, "dinghy_ffi", "start")
pub fn start() -> Nil

pub type ClusterStartResult {
  ClusterStartResult(started: List(ServerId), not_started: List(ServerId))
}

@external(erlang, "dinghy_ffi", "start_cluster")
pub fn start_cluster(
  name: ClusterName,
  function: fn(a, b) -> b,
  initial_state: b,
  servers: List(ServerId),
) -> Result(ClusterStartResult, Nil)

@external(erlang, "dinghy_ffi", "start_or_restart_cluster")
pub fn start_or_restart_cluster(
  name: ClusterName,
  function: fn(a, b) -> b,
  initial_state: b,
  servers: List(ServerId),
) -> Result(ClusterStartResult, Nil)

@external(erlang, "dinghy_ffi", "start_server")
pub fn start_server(
  name: ClusterName,
  function: fn(a, b) -> b,
  initial_state: b,
  servers: List(ServerId),
) -> Result(Nil, Dynamic)

pub type Error {
  Timeout
  Other(Dynamic)
}

@external(erlang, "dinghy_ffi", "process_command")
pub fn process_command(
  server: ServerId,
  command: a,
  timeout: Int,
) -> Result(#(b, ServerId), Error)

@external(erlang, "dinghy_ffi", "members_from_name")
pub fn members_from_name(
  name: ClusterName,
  timeout: Int,
) -> Result(#(List(ServerId), ServerId), Error)

@external(erlang, "dinghy_ffi", "members")
pub fn members(
  servers: List(ServerId),
  timeout: Int,
) -> Result(#(List(ServerId), ServerId), Error)

@external(erlang, "dinghy_ffi", "members_local")
pub fn members_local(
  server: ServerId,
  timeout: Int,
) -> Result(#(List(ServerId), ServerId), Error)

@external(erlang, "dinghy_ffi", "add_member")
pub fn add_member(
  existing: ServerId,
  new_member: ServerId,
  timeout: Int,
) -> Result(Nil, Error)

@external(erlang, "dinghy_ffi", "server_id_node")
pub fn server_id_node(server_id: ServerId) -> Node

pub type QueryResult(a) {
  QueryResult(result: a, leader: ServerId)
}

@external(erlang, "dinghy_ffi", "query_leader")
pub fn query_leader(
  server: ServerId,
  fun: fn(a) -> b,
  timeout: Int,
) -> Result(QueryResult(b), Error)

@external(erlang, "dinghy_ffi", "query_replica")
pub fn query_replica(
  server: ServerId,
  fun: fn(a) -> b,
  timeout: Int,
) -> Result(QueryResult(b), Error)

@external(erlang, "dinghy_ffi", "leave_and_terminate")
pub fn leave_and_terminate(
  server: ServerId,
  to_remove: ServerId,
  timeout: Int,
) -> Result(Nil, Error)

@external(erlang, "dinghy_ffi", "trigger_election")
pub fn trigger_election(server: ServerId, timeout: Int) -> Nil

@external(erlang, "dinghy_ffi", "delete_cluster")
pub fn delete_cluster(
  server: ServerId,
  timeout: Int,
) -> Result(ServerId, Dynamic)
