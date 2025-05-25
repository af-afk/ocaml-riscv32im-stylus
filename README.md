
# RISCV32IM Stylus Simulator

This code simulates Stylus code with a forked SDK that also generates RISCV32IM code. It
uses environment calls to simulate the Ethereum storage tree and the entry calldata. It
does not support calling out yet or any other operations. In fact, most things are
incomplete, including said environment calls! This is a WIP.

## TODO

- [ ] Decode ELF and work with it properly.

I'm having issues linking against libelf on my computer. I don't want to have to handroll
a ELF reader, and the alternatives seem to not be albe to be built, or only work with
ELF64 and a specific target.

- [ ] Storage operations

- [ ] Ethereum environment calls
