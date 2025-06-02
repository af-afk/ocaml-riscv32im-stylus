
# RISCV32IM Stylus Simulator

WARNING! This code does no checking for architecture type, endianness, blah blah!

This code simulates Stylus code with a forked SDK that also generates RISCV32IM code. It
uses environment calls to simulate the Ethereum storage tree and the entry calldata. It
does not support calling out yet or any other operations. In fact, most things are
incomplete! This is a WIP.

## TODO

- [ ] Test everything (duh!)

There are no tests right now. I'll bootstrap off the official testing framework to help
with this.

- [ ] Figure out how to simulate a proof using Risc Zero. See if I can get a prover onto
Ethereum mainnet.
