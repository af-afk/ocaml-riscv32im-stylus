
(*
 * All of these tests implicitly use use nonzero registers!
 *)

open OUnit2

open Riscv32im_stylus

open Storage

let q2o = QCheck_ounit.to_ounit2_test

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
          sim := Simulator.step ~fmt:(make_ounit_formatter ctx) !sim
        done;
        (* This will never get here if something goes wrong unfortunately. *)
        assert_failure "Didn't exit!"
      with Control.Exited `Ebreak -> ()
    )

let addi = q2o @@ QCheck2.Test.make
    ~name:"Addi"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Addi)
    (fun { x ; y ; res ; _ } -> res = Int32.add x y)

let slti = q2o @@ QCheck2.Test.make
    ~name:"Slti"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Slti)
    (fun { x ; y ; res ; _ } -> res = if x < y then 1l else 0l)

let sltiu = q2o @@ QCheck2.Test.make
    ~name:"Sltiu"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Sltiu)
    (fun { x ; y ; res ; _ } ->
       res = if Int32.unsigned_compare x y < 0 then 1l else 0l
    )

let andi = q2o @@ QCheck2.Test.make
    ~name:"Andi"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Andi)
    (fun { x ; y ; res ; _ } -> res = Int32.logand x y)

let ori = q2o @@ QCheck2.Test.make
    ~name:"Ori"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Ori)
    (fun { x ; y ; res ; _ } -> Int32.logor x y = res)

let xori = q2o @@ QCheck2.Test.make
    ~name:"Xori"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Xori)
    (fun { x ; y ; res ; _ } -> Int32.logxor x y = res)

let jalr =
  let open Simulator in
  q2o @@ QCheck2.Test.make
    ~name:"Jalr"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Jalr)
    (fun  { x ; y ; before_s = { pc = before_pc; _ } ; after_s = { pc = after_pc; _ } ; res  ; _  } ->
       let exp_pc = Int32.(logand (add x y) (lognot 1l)) in
       let exp_res = Int32.add before_pc 4l in
       after_pc = exp_pc && res = exp_res
    )

let slli = q2o @@ QCheck2.Test.make
    ~name:"Slli"
    ~print:sprint_gen_i_registers_vals
    (gen_i_shift_registers_and_values `Slli)
    (fun { x ; y ; res ; _ } ->
       let amt = Int32.(to_int (logand y 0x1fl)) in
       Int32.shift_left x amt = res
    )

let srli = q2o @@ QCheck2.Test.make
    ~name:"Srli"
    ~print:sprint_gen_i_registers_vals
    (gen_i_shift_registers_and_values `Srli)
    (fun { x ; y ; res ; _ } ->
       let amt = Int32.(to_int (logand y 0x1fl)) in
       Int32.shift_right_logical x amt = res
    )

let srai = q2o @@ QCheck2.Test.make
    ~name:"Srai"
    ~print:sprint_gen_i_registers_vals
    (gen_i_shift_registers_and_values `Srai)
    (fun { x ; y ; res ; _ } ->
       let amt = Int32.(to_int (logand y 0x1fl)) in
       Int32.shift_right x amt = res)

let add = q2o @@ QCheck2.Test.make
    ~name:"Add"
    ~print:sprint_gen_r_registers_vals
    (gen_r_registers_and_values `Add)
    (fun { x ; y ; src1 ; src2 ; res ; _ } ->
       QCheck2.assume (not (Registers.equal_reg src1 src2));
       res = Int32.add x y
    )

let sub = q2o @@ QCheck2.Test.make
    ~name:"Sub"
    ~print:sprint_gen_r_registers_vals
    (gen_r_registers_and_values `Sub)
    (fun { x ; y ; src1 ; src2 ; res ; _ } ->
       QCheck2.assume (not (Registers.equal_reg src1 src2));
       res = Int32.sub x y
    )

let xor = q2o @@ QCheck2.Test.make
    ~name:"Xor"
    ~print:sprint_gen_r_registers_vals
    (gen_r_registers_and_values `Xor)
    (fun { x ; y ; src1 ; src2 ; res ; _ } ->
       QCheck2.assume (not (Registers.equal_reg src1 src2));
       res = Int32.logxor x y
    )

let mul = q2o @@ QCheck2.Test.make
    ~name:"Mul"
    ~print:sprint_gen_r_registers_vals
    (gen_r_registers_and_values `Mul)
    (fun { x ; y ; src1 ; src2 ; res ; _ } ->
       QCheck2.assume (not (Registers.equal_reg src1 src2));
       res = Int32.mul x y
    )

let mulhu = q2o @@ QCheck2.Test.make
    ~name:"Mulhu"
    ~print:sprint_gen_r_registers_vals
    (gen_r_registers_and_values `Mulhu)
    (fun { x ; y ; src1 ; src2 ; res ; _ } ->
       let open Stdint in
      (*
       * We use stdint as the reference here since we need to be sure about our
       * approach.
       *)
       QCheck2.assume (not (Registers.equal_reg src1 src2));
       let x_uint32 = Uint32.of_int32 x in
       let y_uint32 = Uint32.of_int32 y in
       let x_uint64 = Uint64.of_uint32 x_uint32 in
       let y_uint64 = Uint64.of_uint32 y_uint32 in
       let result64 = Uint64.mul x_uint64 y_uint64 in
       let upper32 = Uint64.shift_right result64 32 |> Uint64.to_uint32 in
       compare upper32 (Uint32.of_int32 res) = 0
    )

let slt = q2o @@ QCheck2.Test.make
    ~name:"Slt"
    ~print:sprint_gen_r_registers_vals
    (gen_r_registers_and_values `Slt)
    (fun { x ; y ; src1 ; src2 ; res ; _ } ->
       QCheck2.assume (not (Registers.equal_reg src1 src2));
       res = if Int32.compare x y < 0 then 1l else 0l
    )

let jal =
  let open Simulator in
  q2o @@ QCheck2.Test.make
    ~name:"Jal"
    ~print:sprint_gen_j_vals
    (gen_j_values `Jal)
    (fun { imm ; before_s = { pc = before_pc; _ } ; after_s = { pc = after_pc; _ } ; res ; _ } ->
       let exp_pc = Int32.(logand (add before_pc imm) (lognot 1l)) in
       let exp_res = Int32.add before_pc 4l in
       after_pc = exp_pc && res = exp_res
    )

let auipc =
  let open Simulator in
  q2o @@ QCheck2.Test.make
    ~name:"Auipc"
    ~print:sprint_gen_u_vals
    (gen_u_values `Auipc)
    (fun { imm ; before_s = { pc = before_pc; _ } ; res ; _ } ->
       let exp = Int32.(add before_pc imm) in
       res = exp
    )

let lui = q2o @@ QCheck2.Test.make
    ~name:"Lui"
    ~print:sprint_gen_u_vals
    (gen_u_values `Lui)
    (fun { imm ; res ; _ } ->
       let cleared = Int32.logand res 0xFFFl in
       res = imm && cleared = 0l
    )

let beq =
  let open Simulator in
  q2o @@ QCheck2.Test.make
    ~name:"Beq"
    ~print:sprint_gen_b_vals
    (gen_b_registers_and_values `Beq)
    (fun { src1 ; src2 ; imm ; x ; y ; before_s = { pc = before_pc; _ } ; after_s = { pc = after_pc ; _  } ; _ } ->
       QCheck2.assume (not (Registers.equal_reg src1 src2));
       if x = y then after_pc = before_pc + imm else after_pc = before_pc + 4l
    )

let bne =
  let open Simulator in
  q2o @@ QCheck2.Test.make
    ~name:"Beq"
    ~print:sprint_gen_b_vals
    (gen_b_registers_and_values `Bne)
    (fun { src1 ; src2 ; imm ; x ; y ; before_s = { pc = before_pc; _ } ; after_s = { pc = after_pc ; _  } ; _ } ->
       QCheck2.assume (not (Registers.equal_reg src1 src2));
       if not (x = y) then after_pc = before_pc + imm else after_pc = before_pc + 4l
    )

let blt =
  let open Simulator in
  q2o @@ QCheck2.Test.make
    ~name:"Blt"
    ~print:sprint_gen_b_vals
    (gen_b_registers_and_values `Blt)
    (fun { src1 ; src2 ; imm ; x ; y ; before_s = { pc = before_pc; _ } ; after_s = { pc = after_pc ; _  } ; _ } ->
       QCheck2.assume (not (Registers.equal_reg src1 src2));
       if not (x > y) then after_pc = before_pc + imm else after_pc = before_pc + 4l
    )

let bltu =
  let open Simulator in
  q2o @@ QCheck2.Test.make
    ~name:"Bltu"
    ~print:sprint_gen_b_vals
    (gen_b_registers_and_values `Bltu)
    (fun { src1 ; src2 ; imm ; x ; y ; before_s = { pc = before_pc; _ } ; after_s = { pc = after_pc ; _  } ; _ } ->
       QCheck2.assume (not (Registers.equal_reg src1 src2));
       if (Int32.unsigned_compare x y < 0) then
         after_pc = before_pc + imm
       else after_pc = before_pc + 4l
    )

let lw =
  let open QCheck2.Gen in
  q2o @@ QCheck2.Test.make
    ~name:"Lw"
    ~print:(fun (addr, word, src, dst, sim, imm) ->
        Format.asprintf "Addr: %d, word: %ld, src: %a, dst: %a, sim: %a, imm: %ld"
          addr
          word
          Registers.pp_reg src
          Registers.pp_reg dst
          Simulator.pp sim
          imm
      )
    (
      let* r = Registers.gen in
      let* pc = int32 in
      let* src = Registers.gen_reg_nonzero in
      let* dst = Registers.gen_reg_nonzero in
      let* imm = Lifted.gen_imm in
      let* word = int32 in
      let* addr, mem = gen_random_memory_access Memory.gen in
      let r = Registers.update r src (Int32.of_int addr) in
      let sim = Simulator.make ~r ~pc ~b:mem () in
      return (addr, word, src, dst, sim, imm)
    )
    (fun (addr, word, src, dst, sim, _) ->
       Memory.store_word sim.b addr word;
       let op = Lifted.(Lw { i_typ_dst = dst ; i_typ_src = src ; i_typ_imm = 0l } ) in
       let Simulator.{ r ; _ } = Simulator.step_lifted sim op in
       Registers.get r dst = word
    )

let ecall = q2o @@ QCheck2.Test.make
    ~name:"Ecall"
    (
      let open QCheck2.Gen in
      let* r = Registers.gen in
      let s = Simulator.make ~r () in
      return s
    )
    (fun s ->
       let op = Lifted.(Ecall empty_i_typ_sys) in
       let e = Int32.to_int (Encoding.encode op) in
       assert_equal (Lifted.from_word 0l e) op;
       let Simulator.{ r ; _ } = s in
       try Simulator.(test_last_op (step_lifted s op) op); true with
       (* We only break if this is set to anything other than zero! *)
       | _ when not (Registers.get r `A7 = 0l) -> true
       | err -> raise err;
    )

let sw =
  let open QCheck2.Gen in
  q2o @@ QCheck2.Test.make
    ~name:"Sw"
    ~print:(fun (addr, src1, src2, word, sim) ->
        Format.asprintf "Addr: %d, src1: %a, src: %a, word: %ld, sim: %a"
          addr
          Registers.pp_reg src1
          Registers.pp_reg src2
          word
          Simulator.pp sim
      )
    (
      let* r = Registers.gen in
      let* pc = int32 in
      let* src1 = Registers.gen_reg_nonzero in
      let* src2 = Registers.gen_reg_nonzero in
      let* addr, mem = gen_random_memory_access Memory.gen in
      let* word = int32 in
      let r =
        Registers.(update (update r src1 (Int32.of_int addr)) src2 word) in
      let sim = Simulator.make ~r ~pc ~b:mem () in
      return (addr, src1, src2, word, sim)
    )
    (fun (addr, src1, src2, word, sim) ->
       QCheck2.assume (not (Registers.equal_reg src1 src2));
       let op = Lifted.(Sw { s_typ_src1 = src1 ; s_typ_src2 = src2 ; s_typ_imm = 0l } ) in
       assert_equal op (Lifted.from_word 0l (Int32.to_int (Encoding.encode op)));
       let Simulator.{ r ; b ; _ } = Simulator.step_lifted sim op in
       let addr = Int32.of_int addr in
       assert_equal addr (Registers.get r src1);
       assert_equal word (Registers.get r src2);
       assert_equal word (Memory.load_word b (Int32.to_int addr));
       true
    )

let sh =
  let open QCheck2.Gen in
  q2o @@ QCheck2.Test.make
    ~name:"Sh"
    ~print:(fun (addr, src1, src2, word, sim) ->
        Format.asprintf "Addr: %d, src1: %a, src: %a, word: %ld, sim: %a"
          addr
          Registers.pp_reg src1
          Registers.pp_reg src2
          word
          Simulator.pp sim
      )
    (
      let* r = Registers.gen in
      let* pc = int32 in
      let* src1 = Registers.gen_reg_nonzero in
      let* src2 = Registers.gen_reg_nonzero in
      let* addr, mem = gen_random_memory_access Memory.gen in
      let* word = int32 in
      let r =
        Registers.(update (update r src1 (Int32.of_int addr)) src2 word) in
      let sim = Simulator.make ~r ~pc ~b:mem () in
      return (addr, src1, src2, word, sim)
    )
    (fun (addr, src1, src2, word, sim) ->
       QCheck2.assume (not (Registers.equal_reg src1 src2));
       let op = Lifted.(Sh { s_typ_src1 = src1 ; s_typ_src2 = src2 ; s_typ_imm = 0l } ) in
       assert_equal op (Lifted.from_word 0l (Int32.to_int (Encoding.encode op)));
       let Simulator.{ r ; b ; _ } = Simulator.step_lifted sim op in
       let addr = Int32.of_int addr in
       assert_equal addr (Registers.get r src1);
       assert_equal word (Registers.get r src2);
       let exp = Int32.logand word 0x0000ffffl in
       let exp_sign_extended = Int32.shift_right (Int32.shift_left exp 16) 16 in
       assert_equal exp_sign_extended (Memory.load_halfword b (Int32.to_int addr));
       assert_equal exp (Memory.load_halfword_unsigned_from_sim b addr);
       true
    )

let test risc_hello_world stack_top pc =
  "Opcodes"
  >:::[ should_simulate_program_ok risc_hello_world stack_top pc
      ; addi
      ; slti
      ; sltiu
      ; andi
      ; ori
      ; xori
      ; jalr
      ; slli
      ; srli
      ; srai
      ; add
      ; sub
      ; xor
      ; mul
      ; mulhu
      ; slt
      ; jal
      ; auipc
      ; lui
      ; beq
      ; bne
      ; blt
      ; bltu
      ; lw
      ; ecall
      ; sw
      ; sh
      ]
