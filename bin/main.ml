
open Riscv32im_stylus

let max_cycles = 1000

module Cycle_detection = struct
  (*
   * VERY poor man's cycle detection that we use during our test
   * application before bootstrapping RISC-V's actual test suite.
   *)
  type t = ((int32 * Registers.t) * int) list [@@deriving show]

  let empty: t = []

  let track t pc r =
    let key = (pc, r) in
    let rec update_list = function
      | [] -> [(key, 1)]
      | (k, count) :: rest when k = key ->
          if count + 1 > max_cycles then (
            Registers.pp Format.err_formatter r;
            Format.(pp_force_newline err_formatter ());
            failwith "Cycle detected!"
          ) else
            (key, count + 1) :: rest
      | item :: rest -> item :: update_list rest
    in
    let updated = update_list t in
    let rec take n = function
      | [] -> []
      | x :: xs when n > 0 -> x :: take (n - 1) xs
      | _ -> []
    in
    take 10 updated
end

let () =
  let poor_log = Format.err_formatter in
  let mem, stack_top, pc = Memory.of_path (Array.get Sys.argv 1) in
  let registers = { Registers.empty_spike with t_r_sp = stack_top } in
  let sim = ref (
      Simulator.make
        ~b:mem
        ~r:registers
        ~pc
        ~e:Ethereum.empty
        ~cd_b:Calldata.empty
        ~rd_b:Calldata.empty) in
  let cycles = ref Cycle_detection.empty in
  Sys.set_signal
    Sys.sigusr1
    (Sys.Signal_handle (fun _ ->
         Simulator.pp Format.err_formatter !sim;
         Format.pp_print_newline Format.err_formatter ();
         Cycle_detection.pp Format.err_formatter !cycles;
         Format.pp_print_newline Format.err_formatter ()
       ));
  while true do
    sim := Simulator.step poor_log !sim;
    let Simulator.{ pc; r; _ } = !sim in
    cycles := Cycle_detection.track !cycles pc r
  done
