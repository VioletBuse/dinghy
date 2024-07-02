import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/string

pub type StateMachine {
  StateMachine(current_version: Int, version_modules: Dict(Int, Module))
}

pub type StateMachineVersion(init_args, state, message) {
  StateMachineVersion(
    init: fn(init_args) -> state,
    apply: fn(message, state) -> #(state, Nil, List(SideEffect)),
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

pub fn apply_factory(
  version: StateMachineVersion(init_arg, state, message),
) -> fn(Dynamic, state) -> #(state, Nil, List(SideEffect)) {
  fn(message, state) { todo }
}
