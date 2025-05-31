
open Riscv32im_stylus

let () =
  let mem = Memory.of_path "/home/user/Documents/markov-geist-research/risc-hello-world/target/riscv32im-unknown-none-elf/debug/risc-hello-world" in
  let sim = Simulator.make ~b:mem ~r:Registers.empty ~pc:0x80000000l in
  Simulator.step sim
