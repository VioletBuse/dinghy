import dinghy/side_effects.{type SideEffect}
import gleam/dynamic.{type Dynamic}
import gleam/erlang
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist
import gleam/erlang/node.{type Node}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/result

pub type ServerId

pub type ClusterName

pub type Cluster(state, message) {
  Cluster(
    name: ClusterName,
    servers: List(ServerId),
    initial_state: state,
    handler: fn(Operation(message), state) -> #(state, List(SideEffect)),
  )
}

pub fn start() {
  erlang.ensure_all_started(atom.create_from_string("ra"))
  |> result.map(fn(_) { Nil })
}

@external(erlang, "dinghy_ffi", "identity")
fn cluster_name_ffi(in: a) -> ClusterName

pub fn cluster_name(name: String) -> ClusterName {
  charlist.from_string(name)
  |> cluster_name_ffi
}

pub fn cluster_name_from_atom(name: Atom) -> ClusterName {
  cluster_name_ffi(name)
}

@external(erlang, "dinghy_ffi", "identity")
fn server_id_ffi(in: #(a, Node)) -> ServerId

pub fn server_id(name: ClusterName, node: Node) -> ServerId {
  server_id_ffi(#(name, node))
}

pub fn server_id_atom(name: Atom, node: Node) -> ServerId {
  server_id_ffi(#(name, node))
}

pub type Operation(message_type) {
  Operation(message_type)
  Timeout
  ProcessDown(Pid)
  NodeDown(Node)
  NodeUp(Node)
}

pub type RaState {
  Leader
  Follower
  Candidate
  PreVote
  AwaitCondition
  DeleteAndTerminate
  TerminatingLeader
  TerminatingFollower
  Recover
  Recovered
  Stop
  ReceiveSnapshot
  Eol
}

@external(erlang, "dinghy_state_machine", "start")
fn start_cluster_ffi(
  name: ClusterName,
  initial_state: state,
  handler_function: fn(Dynamic, state) -> #(state, List(SideEffect)),
  state_enter: fn(RaState, state) -> List(SideEffect),
  members: List(ServerId),
) -> Result(#(List(ServerId), List(ServerId)), Nil)

@external(erlang, "dinghy_state_machine", "create_handler_function")
fn create_handler_function(
  input: fn(Operation(message), state) -> #(state, List(SideEffect)),
) -> fn(Dynamic, state) -> #(state, List(SideEffect))

pub fn start_cluster(
  name: ClusterName,
  initial_state: state,
  handler: fn(Operation(message), state) -> #(state, List(SideEffect)),
  servers: List(ServerId),
) -> Result(Cluster(state, message), Nil) {
  use #(started, _) <- result.try(start_cluster_ffi(
    name,
    initial_state,
    create_handler_function(handler),
    fn(_, _) { [] },
    servers,
  ))

  Ok(Cluster(name, started, initial_state, handler))
}
