import decipher
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/node.{type Node}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/option.{type Option, None, Some}
import gleam/string

pub type StateMachine {
  StateMachine(current_version: Int, version_modules: Dict(Int, Module))
}

pub type NodeMonitorEvent {
  NodeUp(Node)
  NodeDown(Node)
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

pub type StateMachineVersion(init_args, state, message) {
  StateMachineVersion(
    init: fn(init_args) -> state,
    handle_message: fn(message, state) -> #(state, List(SideEffect)),
    handle_timeout: Option(fn(Dynamic, state) -> #(state, List(SideEffect))),
    handle_node_monitor: Option(
      fn(NodeMonitorEvent, state) -> #(state, List(SideEffect)),
    ),
    handle_process_down: Option(fn(Pid, state) -> #(state, List(SideEffect))),
    handle_state_enter: Option(fn(RaState, state) -> List(SideEffect)),
    handle_tick: Option(fn(Int, state) -> List(SideEffect)),
  )
}

pub type SideEffect {
  SideEffect
}

pub opaque type Module {
  Module(name: Atom)
}

pub fn gleam_module(name: String) -> Module {
  string.replace(name, each: "/", with: "@")
  |> atom.create_from_string
  |> Module
}

pub fn version_factory(machine: StateMachine) -> fn() -> Int {
  fn() { machine.current_version }
}

pub fn which_module_factory(machine: StateMachine) -> fn(Int) -> Atom {
  fn(version) {
    let assert Ok(Module(name)) = dict.get(machine.version_modules, version)
    name
  }
}

pub fn init_factory(
  version: StateMachineVersion(init_arg, state, message),
) -> fn(init_arg) -> state {
  version.init
}

type InternalCommandType(message) {
  IntUserCommand(message)
  IntProcDown(Pid)
  IntNodeDown(Atom)
  IntNodeUp(Atom)
  IntTimeout(Dynamic)
}

@external(erlang, "dinghy_ffi", "identity")
fn atom_to_node(atom: Atom) -> Node

@external(erlang, "dinghy_ffi", "identity")
fn dyn_to_message(dyn: Dynamic) -> Result(message, dynamic.DecodeErrors)

pub fn apply_factory(
  version: StateMachineVersion(init_arg, state, message),
) -> fn(Dynamic, Dynamic, state) -> #(state, Nil, List(SideEffect)) {
  fn(_, message, state) {
    let down_atom = atom.create_from_string("down")
    let node_up_atom = atom.create_from_string("nodeup")
    let node_down_atom = atom.create_from_string("nodedown")
    let timeout_atom = atom.create_from_string("timeout")

    let command_decoder: fn(Dynamic) ->
      Result(InternalCommandType(message), dynamic.DecodeErrors) =
      dynamic.any([
        decipher.tagged_union(dynamic.element(0, atom.from_dynamic), [
          #(
            down_atom,
            dynamic.decode1(
              IntProcDown,
              dynamic.element(1, process.pid_from_dynamic),
            ),
          ),
          #(
            node_up_atom,
            dynamic.decode1(IntNodeUp, dynamic.element(1, atom.from_dynamic)),
          ),
          #(
            node_down_atom,
            dynamic.decode1(IntNodeDown, dynamic.element(1, atom.from_dynamic)),
          ),
          #(
            timeout_atom,
            dynamic.decode1(IntTimeout, dynamic.element(1, dynamic.dynamic)),
          ),
        ]),
        fn(val) { dynamic.decode1(IntUserCommand, dyn_to_message)(val) },
      ])

    let assert Ok(command) = command_decoder(message)

    let #(new_state, effects) = case
      command,
      version.handle_timeout,
      version.handle_process_down,
      version.handle_node_monitor
    {
      IntUserCommand(msg), _, _, _ -> version.handle_message(msg, state)
      IntProcDown(pid), _, Some(process_monitor), _ ->
        process_monitor(pid, state)
      IntNodeDown(node), _, _, Some(node_monitor) ->
        node_monitor(NodeDown(atom_to_node(node)), state)
      IntNodeUp(node), _, _, Some(node_monitor) ->
        node_monitor(NodeUp(atom_to_node(node)), state)
      IntTimeout(term), Some(timeout_handler), _, _ ->
        timeout_handler(term, state)
      _, _, _, _ -> #(state, [])
    }

    #(new_state, Nil, effects)
  }
}

pub fn state_enter_factory(
  version: StateMachineVersion(init_arg, state, message),
) -> fn(RaState, state) -> List(SideEffect) {
  fn(ra_state, state) {
    case version.handle_state_enter {
      Some(fun) -> fun(ra_state, state)
      None -> []
    }
  }
}

pub fn tick_factory(
  version: StateMachineVersion(init_arg, state, message),
) -> fn(Int, state) -> List(SideEffect) {
  fn(ms, state) {
    case version.handle_tick {
      Some(fun) -> fun(ms, state)
      None -> []
    }
  }
}
