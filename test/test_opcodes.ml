
open Riscv32im_stylus

let make_ounit_formatter ctx =
  Format.make_formatter
    (fun s pos len ->
       let msg = String.sub s pos len in
       (* This is only written to by the logger ecall. *)
       OUnit2.logf ctx `Info "%s" msg)
    (fun () -> ())

let should_simulate_program_ok mem stack_top pc =
  let open OUnit2 in
  "should simulate reference" >:: (fun ctx ->
      try
        let registers = { Registers.empty_spike with t_r_sp = Int32.of_int stack_top } in
        let sim = ref (
            Simulator.make
              ~b:mem
              ~r:registers
              ~pc
              ~e:Ethereum.empty
              ~cd_b:Calldata.empty
              ~rd_b:Calldata.empty) in
        while true do
          sim := Simulator.step (make_ounit_formatter ctx) !sim
        done;
        (* This will never get here if something goes wrong unfortunately. *)
        assert_failure "Didn't exit!"
      with Control.Exited `Ebreak -> ()
    )

let test risc_hello_world stack_top pc =
  let open OUnit2 in
  "opcodes"
  >:::[ should_simulate_program_ok risc_hello_world stack_top pc ]
