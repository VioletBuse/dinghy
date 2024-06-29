import dinghy/internal/ffi.{type ClusterName, type ServerId}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/node.{type Node}
import gleam/list
import gleam/result

fn local_replica(servers: List(ServerId)) -> Result(ServerId, Nil) {
  list.find(servers, fn(server) {
    let node = ffi.server_id_node(server)
    node == node.self()
  })
  |> result.try_recover(fn(_) {
    case servers {
      [] -> Error(Nil)
      [first, ..] -> Ok(first)
    }
  })
}

pub type Cluster(message, state) {
  Cluster(name: ClusterName)
}

pub fn start_cluster(
  name: Atom,
  state: a,
  handle_message: fn(b, a) -> a,
  nodes: List(Node),
) {
  let cluster_name = ffi.cluster_name(name)
  let nodes = list.map(nodes, ffi.server_id(cluster_name, _))
  let cluster_start_result =
    ffi.start_cluster(cluster_name, handle_message, state, nodes)

  use _ <- result.try(cluster_start_result)

  let cluster: Cluster(b, a) = Cluster(cluster_name)

  Ok(cluster)
}

pub fn send(
  cluster: Cluster(msg, s),
  message: msg,
  timeout: Int,
) -> Result(s, ffi.Error) {
  use #(_, leader) <- result.try(ffi.members_from_name(cluster.name, timeout))
  use #(state, _) <- result.try(ffi.process_command(leader, message, timeout))
  Ok(state)
}

pub fn query_replica(
  cluster: Cluster(msg, s),
  function: fn(s) -> a,
  timeout: Int,
) -> Result(a, ffi.Error) {
  use #(replicas, _) <- result.try(ffi.members_from_name(cluster.name, timeout))
  use replica <- result.try(
    local_replica(replicas) |> result.map_error(fn(_) { ffi.Timeout }),
  )
  use ffi.QueryResult(res, _leader) <- result.try(ffi.query_replica(
    replica,
    function,
    timeout,
  ))

  Ok(res)
}

pub fn query_leader(
  cluster: Cluster(msg, s),
  function: fn(s) -> a,
  timeout: Int,
) -> Result(a, ffi.Error) {
  use #(_, leader) <- result.try(ffi.members_from_name(cluster.name, timeout))
  use ffi.QueryResult(res, _leader) <- result.try(ffi.query_leader(
    leader,
    function,
    timeout,
  ))

  Ok(res)
}
