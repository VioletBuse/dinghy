import gleam/erlang.{type TimeUnit}
import gleam/erlang/node.{type Node}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/otp/task
import gleam/result

type PingResult {
  Pong
  Pang
}

@external(erlang, "timer", "tc")
fn measure_time(fxn: fn() -> a, unit: TimeUnit) -> #(Int, a)

@external(erlang, "net_adm", "ping")
fn ping(node: Node) -> PingResult

pub fn latency_to_node(node: Node) -> Result(Int, Nil) {
  let #(time, result) = measure_time(fn() { ping(node) }, erlang.Millisecond)

  case result {
    Pang -> Error(Nil)
    Pong -> Ok(time)
  }
}

fn latency_task(node: Node) {
  task.async(fn() { latency_to_node(node) })
}

pub fn get_closest_node(nodes: List(Node), timeout: Int) -> Result(Node, Nil) {
  let tasks =
    nodes
    |> list.map(latency_task)

  process.sleep(timeout)

  let results =
    tasks
    |> list.map(task.try_await(_, 1))
    |> list.map(result.nil_error)
    |> list.map(result.flatten)
    |> list.zip(nodes)
    |> list.map(fn(res) { result.map(res.0, fn(time) { #(time, res.1) }) })
    |> result.values
    |> list.sort(fn(x, y) { int.compare(x.0, y.0) })

  case results {
    [] -> Error(Nil)
    [first, ..] -> Ok(first.1)
  }
}
