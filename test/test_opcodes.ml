
(*
 * All of these tests implicitly use use nonzero registers!
 *)

open OUnit2

open Riscv32im_stylus

open Storage

let make_ounit_formatter ctx =
  Format.make_formatter
    (fun s pos len ->
       let msg = String.sub s pos len in
       (* This is only written to by the logger ecall. *)
       OUnit2.logf ctx `Info "%s" msg)
    (fun () -> ())

let should_simulate_program_ok mem stack_top pc =
  "Should simulate reference" >:: (fun ctx ->
      try
        let registers = { Registers.empty_spike with t_r_sp = Int32.of_int stack_top } in
        let sim = ref (Simulator.make ~b:mem ~r:registers ~pc ()) in
        while true do
          sim := Simulator.step (make_ounit_formatter ctx) !sim
        done;
        (* This will never get here if something goes wrong unfortunately. *)
        assert_failure "Didn't exit!"
      with Control.Exited `Ebreak -> ()
    )

let addi =
  QCheck_ounit.to_ounit2_test @@ QCheck2.Test.make
    ~name:"Addi"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Addi)
    (fun { x ; y ; res ; _ } -> res = Int32.add x y)

let slti =
  QCheck_ounit.to_ounit2_test @@ QCheck2.Test.make
    ~name:"Slti"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Slti)
    (fun { x ; y ; res ; _ } -> res = if x < y then 1l else 0l)

let sltiu =
  QCheck_ounit.to_ounit2_test @@ QCheck2.Test.make
    ~name:"Sltiu"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Sltiu)
    (fun { x ; y ; res ; _ } ->
       res = if Int32.unsigned_compare x y < 0 then 1l else 0l
    )

let andi =
  QCheck_ounit.to_ounit2_test @@ QCheck2.Test.make
    ~name:"Andi"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Andi)
    (fun { x ; y ; res ; _ } -> res = Int32.logand x y)

let ori =
  QCheck_ounit.to_ounit2_test @@ QCheck2.Test.make
    ~name:"Ori"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Ori)
    (fun { x ; y ; res ; _ } -> Int32.logor x y = res)

let xori =
  QCheck_ounit.to_ounit2_test @@ QCheck2.Test.make
    ~name:"Xori"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Xori)
    (fun { x ; y ; res ; _ } -> Int32.logxor x y = res)

let jalr =
  let open Simulator in
  QCheck_ounit.to_ounit2_test @@ QCheck2.Test.make
    ~name:"Jalr"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Jalr)
    (fun
      { x
      ; y
      ; before_s = { pc = before_pc; _ }
      ; after_s = { pc = after_pc; _ }
      ; res
      ; _
      } ->
      let exp_pc = Int32.(logand (add x y) (lognot 1l)) in
      let exp_res = Int32.add before_pc 4l in
      after_pc = exp_pc && res = exp_res
    )

let test risc_hello_world stack_top pc =
  "opcodes"
  >:::[ should_simulate_program_ok risc_hello_world stack_top pc
      ; addi
      ; slti
      ; sltiu
      ; andi
      ; ori
      ; xori
      ; jalr ]
