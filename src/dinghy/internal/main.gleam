import delay
import dinghy/internal/ffi.{type ClusterName, type ServerId}
import gleam/erlang/node.{type Node}
import gleam/list
import gleam/result

pub type ClusterDefinition(name, message, state) {
  ClusterDefinition(
    name: name,
    handler: fn(message, state) -> state,
    initial_state: state,
    nodes: List(Node),
  )
}

pub opaque type Cluster(message, state) {
  Cluster(name: ClusterName, members: List(Member(message, state)))
}

pub opaque type Member(message, state) {
  Member(server_id: ServerId)
}

pub fn define_cluster(
  name: name,
  initial_state: state,
  handler: fn(message, state) -> state,
  nodes: List(Node),
) {
  ClusterDefinition(name, handler, initial_state, nodes)
}

pub fn start(
  definition: ClusterDefinition(name, message, state),
) -> Result(Cluster(message, state), Nil) {
  let name = ffi.cluster_name(definition.name)
  let server_ids = list.map(definition.nodes, ffi.server_id(name, _))

  case
    ffi.start_cluster_ffi(
      name,
      definition.handler,
      definition.initial_state,
      server_ids,
    )
  {
    Ok(ffi.ClusterStartResult(started, failed)) -> {
      let members = list.map(started, Member)

      Ok(Cluster(name, members))
    }
    _ -> Error(Nil)
  }
}
