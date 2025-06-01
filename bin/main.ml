
open Riscv32im_stylus

let () =
  let mem, stack_top, pc =
    Memory.of_path
      "/home/user/Documents/markov-geist-research/risc-hello-world/target/riscv32im-unknown-none-elf/debug/risc-hello-world" in
  let registers = { Registers.empty_spike with t_r_sp = stack_top } in
  let sim = ref (Simulator.make ~b:mem ~r:registers ~pc) in
  Sys.set_signal
    Sys.sigusr1
    (Sys.Signal_handle (fun _ ->
         Simulator.pp Format.err_formatter !sim;
         Format.pp_print_newline Format.err_formatter ()
       ));
  while true do
    sim := Simulator.step !sim
  done
