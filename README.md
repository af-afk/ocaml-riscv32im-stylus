
# RISCV32IM Stylus Simulator

This code simulates RISCV32IM code, with some supported ecalls for EVM and Stylus-specific
operations. It's intended to be used with Arbitrum Stylus, with a forked SDK.

## Installation

To build the simulator, the following is needed:

- OCaml

You can install the OCaml programming language using a OS package manager easily using
instructions from the [website](http://ocaml.org/).

- Dune

You can install the Dune package manager with [these instructions](https://dune.build/install).

- binutils-dev

It's best to figure out how to do this based on what you use locally, on Debian:

	sudo apt-get install -y binutils-dev

## Building

	dune build bin

This builds the executable, located at `_build/default/bin/main.exe`. The command line
tool takes a smple path to load. The program can exit by using EBREAK.

## Usage

The simplest use is with a test executable, test/risc-hello-world:

	./_build/default/bin/main.exe test/risc-hello-world $(cast calldata 'hello()')

This contains the contract code:

```rust
#[entrypoint]
#[storage]
struct Storage {
    pub message: StorageString,
}

#[public]
impl Storage {
    pub fn hello(&mut self) -> String {
        self.message.set_str("Hello!");
        self.message.get_string()
    }
}
```

## Supported environment calls

The following environment calls are supported:

- [X] Storage loads/stores
- [X] Calldata loading and return data

The following won't be supported:

- Calling
- Sending

## TODOs

- [ ] Read actual program headers instead of sections so stripped binaries work

I had some linking issues that prevented me from using a approach I wanted here. So the
section haeders is the current approach.

- [ ] Tighten up the use of the int to int32 types (this is abit messy)
