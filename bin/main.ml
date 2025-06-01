
open Riscv32im_stylus

let () =
  let mem, stack_top, pc =
    Memory.of_path
      "/home/user/Documents/markov-geist-research/risc-hello-world/target/riscv32im-unknown-none-elf/debug/risc-hello-world" in
  let registers = { Registers.empty_spike with t_r_sp = stack_top } in
  let sim = Simulator.make ~b:mem ~r:registers ~pc in
  Simulator.step_forever sim
