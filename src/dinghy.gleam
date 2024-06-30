import dinghy/internal/ffi.{type ClusterName, type ServerId}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/node.{type Node}
import gleam/list
import gleam/result

pub fn start() {
  ffi.start()
}

pub type Cluster(state, message) {
  Cluster(
    name: ClusterName,
    nodes: List(Node),
    initial_state: state,
    handler: fn(message, state) -> state,
  )
}

pub fn start_cluster(
  name: Atom,
  initial_state: state,
  handler: fn(message, state) -> state,
  nodes: List(Node),
) -> Result(Cluster(state, message), Nil) {
  let cluster_name = ffi.cluster_name(name)
  let server_ids = list.map(nodes, ffi.server_id(cluster_name, _))
  let start_result =
    ffi.start_cluster(cluster_name, handler, initial_state, server_ids)

  case start_result {
    Error(_) -> Error(Nil)
    Ok(_) -> Ok(Cluster(cluster_name, nodes, initial_state, handler))
  }
}

pub fn join_cluster(
  cluster: Cluster(state, message),
  existing_node: Node,
  new_node: Node,
) -> Result(Cluster(state, message), ffi.Error) {
  let existing = ffi.server_id(cluster.name, existing_node)
  let new = ffi.server_id(cluster.name, new_node)

  use _ <- result.try(ffi.add_member(existing, new, 1000))
  use _ <- result.try(
    ffi.start_server(cluster.name, cluster.handler, cluster.initial_state, [new])
    |> result.map_error(ffi.Other),
  )

  Ok(Cluster(..cluster, nodes: [new_node, ..cluster.nodes]))
}
