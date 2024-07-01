import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/node.{type Node}
import gleam/erlang/process.{type Pid, type Subject}

type MonitorType {
  Process
  Node
}

pub opaque type SideEffect {
  SendMsg(to: Pid, value: Dynamic, options: Dynamic)
  Monitor(monitor_type: MonitorType, value: Dynamic)
  Demonitor(monitor_type: MonitorType, value: Dynamic)
  ModCall(module: Atom, function: Atom, args: List(Dynamic))
  Timer(name: Dynamic, time: Dynamic)
}

pub fn send_message(to: Subject(value), value: value) -> SideEffect {
  SendMsg(process.subject_owner(to), dynamic.from(value), dynamic.from([]))
}

pub fn monitor_process(process: Pid) -> SideEffect {
  Monitor(Process, dynamic.from(process))
}

pub fn monitor_node(node: Node) -> SideEffect {
  Monitor(Node, dynamic.from(node))
}

pub fn demonitor_process(process: Pid) -> SideEffect {
  Demonitor(Process, dynamic.from(process))
}

pub fn demonitor_node(node: Node) -> SideEffect {
  Demonitor(Node, dynamic.from(node))
}

/// Be careful not to call any long blocking functions here.
/// Long running functions are best run in another process
pub fn call_function(function: fn() -> Nil) -> SideEffect {
  ModCall(atom.create_from_string("erlang"), atom.create_from_string("apply"), [
    dynamic.from(function),
    dynamic.from([]),
  ])
}

pub type Timeout {
  Cancel
  Time(Int)
}

pub fn set_timeout(name: a, time: Timeout) -> SideEffect {
  case time {
    Cancel ->
      Timer(
        dynamic.from(name),
        dynamic.from(atom.create_from_string("infinity")),
      )
    Time(time) -> Timer(dynamic.from(name), dynamic.from(time))
  }
}
