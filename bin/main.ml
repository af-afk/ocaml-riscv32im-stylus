
open Riscv32im_stylus

let () =
  let (mem, stack_top) =
    Memory.of_path
      "/home/user/Documents/markov-geist-research/risc-hello-world/target/riscv32im-unknown-none-elf/debug/risc-hello-world" in
  let registers = { Registers.empty with t_r_sp = stack_top } in
  let sim =
    Simulator.make ~b:mem ~r:registers ~pc:0x80000000l in
  let Simulator.{ r ; _ } = Simulator.step_count sim 10 in
  Printf.eprintf "%s\n" (Registers.show r)
