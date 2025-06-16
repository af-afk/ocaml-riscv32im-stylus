
# RISCV32IM Stylus Simulator

This code simulates RISCV32IM code, with some supported ecalls for EVM and Stylus-specific
operations. It's intended to be used with Arbitrum Stylus, with a forked SDK.

## Installation

To build the simulator, the following is needed:

- OCaml

You can install the OCaml programming language from the
website[http://ocaml.org/](http://ocaml.org/)

- Dune

You can install the Dune package manager with [these intsructions](https://dune.build/install).

- binutils-dev

This can be installed using any major package manager quickly.

## Building

	dune build bin

This builds the executable, located at `_build/default/bin/main.exe`. The command line
tool takes a smple path to load. The program can exit by using EBREAK.

## Usage

The simplest use is with a test executable, test/risc-hello-world:

	./_build/default/bin/main.exe test/risc-hello-world

This should exit normally withoua ny message. To test storage writing and reading:

	./_build/default/bin/main.exe test/storage-write-read-print

A reference repository will be made available soon with instructions on building.

## Supported environment calls

The following environment calls are supported:

- [ ] Storage loads/stores
- [ ] Calldata loading

The following won't be supported:

- [ ] Calling

## TODOs

- [ ] Read actual program headers instead of sections so stripped binaries work

I had some linking issues that prevented me from using a approach I wanted here.

- [ ] Finish basic CSR support with zeroes

- [ ] Functional view over the memory and calldata types

- [ ] Finish testing everything

- [ ] Tighten up the use of the int to int32 types (this is abit messy)
