
open Riscv32im_stylus

let () =
  let (mem, stack_top) =
    Memory.of_path
      "/home/user/Documents/markov-geist-research/risc-hello-world/target/riscv32im-unknown-none-elf/debug/risc-hello-world" in
  (* TODO read this along with the other sections *)
  let pc = 0x800000d4l in
  let registers = { Registers.empty_spike with t_r_sp = stack_top } in
  let sim =
    Simulator.make ~b:mem ~r:registers ~pc in
  Printf.eprintf "Status: %s\n" (Simulator.show sim);
  let s = Simulator.step_count sim 100_000 in
  Printf.eprintf "SUCCESS? %ld %s\n" s.r.Registers.t_r_a0 (Simulator.show s)
