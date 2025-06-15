
open QCheck2.Gen

open Riscv32im_stylus

let gen_random_memory_access m =
  let* m = m in
  let min_addr = List.fold_left (fun acc Memory.Region.{ base; _ } ->
      min acc base) Int.max_int m in
  let max_addr = List.fold_left (fun acc Memory.Region.{ base; size; _ } ->
      max acc (base + size - 1)) Int.min_int m in
  let* addr = int_range min_addr (max_addr - 3) in
  let addr = (addr / 4) * 4 in
  return (addr, m)

let gen_random_word m =
  let* addr, m = gen_random_memory_access m in
  let* w = int32 in
  return (addr, w, m)

let empty_fmt =
  Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

let test_last_op Simulator.{ last_op ; _ } op =
  (*
   * Double check somehow we didn't execute the wrong operation last.
   * Useful if we're working with signed and unsigned op variants.
   *)
  match last_op, op with
  | Some last_op, op when Lifted.equal last_op op -> ()
  | Some last_op, op ->
    failwith (
      Format.asprintf "Last op is not the same! Last op was %a, should be %a"
        Lifted.pp last_op
        Lifted.pp op
    )
  | None, op ->
    failwith (Format.asprintf "Last op was not set! Should be %a" Lifted.pp op)

(* Simple ease of access record as a testing view for the I type. *)
type test_i =
  { o: Lifted.t
  ; r: Registers.t (* This is only a shorthand way of accessing registers. *)
  ; src: Registers.reg
  ; dst: Registers.reg
  ; x: int32
  ; y: int32
  ; before_s: Simulator.t
  ; after_s: Simulator.t
  ; res: int32
  ; mem: Memory.t option
  ; word: int }

let gen_i_registers ~lifted_f ~tag_f tag =
  let open QCheck2.Gen in
  let* x = int32 in
  let* pc = int32 in
  let* y = Lifted.gen_imm in
  let* src = Registers.gen_reg_nonzero in
  let* dst = Registers.gen_reg_nonzero in
  let i = lifted_f dst src y in
  let o = tag_f tag i in
  let* r = Registers.gen in
  let before_s = Simulator.make ~r: Registers.(update r src x) ~pc () in
  let after_s = Simulator.step_lifted before_s o in
  let res = Registers.get after_s.r dst in
  test_last_op after_s o;
  return
    { o
    ; r = after_s.r
    ; src
    ; dst
    ; x
    ; y
    ; before_s
    ; after_s
    ; res
    ; mem = None
    ; word = 0 }

let gen_i_registers_and_values x =
  let open Lifted in
  gen_i_registers
    ~lifted_f:(fun i_typ_dst i_typ_src i_typ_imm -> { i_typ_dst ; i_typ_src ; i_typ_imm })
    ~tag_f:Opcodes.tag_i
    x

let gen_i_shift_registers_and_values x =
  let open Lifted in
  gen_i_registers
    ~lifted_f:(fun i_typ_sft_dst i_typ_sft_src i_typ_sft_imm ->
        { i_typ_sft_dst ; i_typ_sft_src ; i_typ_sft_imm })
    ~tag_f:Opcodes.tag_i_shift
    x

let gen_i_sys_registers_and_values x =
  let open Lifted in
  gen_i_registers
    ~lifted_f:(fun i_typ_sys_dst i_typ_sys_src i_typ_sys_imm ->
        { i_typ_sys_dst ; i_typ_sys_src ; i_typ_sys_imm })
    ~tag_f:Opcodes.tag_i_sys
    x

let sprint_gen_i_registers_vals { src ; dst ; x ; y ; after_s ; res ; word ;_ } =
  Format.asprintf "Val1: %ld, val2: %ld, src: %a, dst: %a, after simulator: %a, result: %ld, word: 0x%x"
    x
    y
    Registers.pp_reg src
    Registers.pp_reg dst
    Simulator.pp after_s
    res
    word

type test_r =
  { o: Lifted.t
  ; r: Registers.t
  ; dst: Registers.reg
  ; src1: Registers.reg
  ; src2: Registers.reg
  ; x: int32
  ; y: int32
  ; before_s: Simulator.t
  ; after_s: Simulator.t
  ; res: int32 }

let gen_r_registers_and_values tag =
  let open QCheck2.Gen in
  let* x = int32 in
  let* y = Lifted.gen_imm in
  let* src1 = Registers.gen_reg_nonzero in
  let* src2 = Registers.gen_reg_nonzero in
  let* dst = Registers.gen_reg_nonzero in
  let* r = Registers.gen in
  let r = Registers.(update (update r src1 x) src2 y) in
  let i = Lifted.{ r_typ_dst = dst; r_typ_src1 = src1; r_typ_src2 = src2 } in
  let o = Opcodes.tag_r tag i in
  let before_s = Simulator.make ~r () in
  let after_s = Simulator.step_lifted before_s o in
  let res = Registers.get after_s.r dst in
  test_last_op after_s o;
  return { o ; r = after_s.r ; src1 ; src2 ; dst ; x ; y ; before_s; after_s; res }

let sprint_gen_r_registers_vals { x ; y ; src1 ; src2 ; dst ; after_s ; res; _} =
  Format.asprintf "X: %ld, y: %ld, src1: %a, src2: %a, dst: %a, after simulator: %a, result: %ld"
    x
    y
    Registers.pp_reg src1
    Registers.pp_reg src2
    Registers.pp_reg dst
    Simulator.pp after_s
    res

type test_j =
  { o: Lifted.t
  ; r: Registers.t
  ; dst: Registers.reg
  ; imm: int32
  ; before_s: Simulator.t
  ; after_s: Simulator.t
  ; res: int32 }

let gen_j_values tag =
  let open QCheck2.Gen in
  let* imm = Lifted.gen_imm_j_typ in
  let* pc = int32 in
  let* dst = Registers.gen_reg_nonzero in
  let* r = Registers.gen in
  let i = Lifted.{ j_typ_dst = dst; j_typ_imm = imm } in
  let o = Opcodes.tag_j tag i in
  let before_s = Simulator.make ~r ~pc () in
  let after_s = Simulator.step_lifted before_s o in
  let res = Registers.get after_s.r dst in
  test_last_op after_s o;
  return { o ; r ; dst ; imm ; before_s ; after_s ; res }

let sprint_gen_j_vals { dst ; imm ; after_s ; res ; _ } =
  Format.asprintf "Imm: %ld, dst: %a, after simulator: %a, result: %ld"
    imm
    Registers.pp_reg dst
    Simulator.pp after_s
    res

type test_u =
  { o: Lifted.t
  ; r: Registers.t
  ; dst: Registers.reg
  ; imm: int32
  ; before_s: Simulator.t
  ; after_s: Simulator.t
  ; res: int32 }

let gen_u_values tag =
  let open QCheck2.Gen in
  let* imm = Lifted.gen_imm_u_typ in
  let* pc = int32 in
  let* dst = Registers.gen_reg_nonzero in
  let* r = Registers.gen in
  let i = Lifted.{ u_typ_dst = dst; u_typ_imm = imm } in
  let o = Opcodes.tag_u tag i in
  let before_s = Simulator.make ~r ~pc () in
  let after_s = Simulator.step_lifted before_s o in
  let res = Registers.get after_s.r dst in
  test_last_op after_s o;
  return { o ; r ; dst ; imm ; before_s ; after_s ; res }

let sprint_gen_u_vals { dst ; imm ; after_s ; res ; _ } =
  Format.asprintf "Imm: %ld, dst: %a, after simulator: %a, result: %ld"
    imm
    Registers.pp_reg dst
    Simulator.pp after_s
    res

type test_b =
  { o: Lifted.t
  ; r: Registers.t
  ; x: int32
  ; y: int32
  ; src1: Registers.reg
  ; src2: Registers.reg
  ; imm: int32
  ; before_s: Simulator.t
  ; after_s: Simulator.t }

let gen_b_registers_and_values tag =
  let open QCheck2.Gen in
  let* imm = Lifted.gen_imm_u_typ in
  let* pc = int32 in
  let* src1 = Registers.gen_reg_nonzero in
  let* src2 = Registers.gen_reg_nonzero in
  let* x = int32 in
  let* y = int32 in
  let* r = Registers.gen in
  let r = Registers.(update (update r src1 x) src2 y) in
  let i = Lifted.{ b_typ_src1 = src1 ; b_typ_src2 = src2 ; b_typ_imm = imm } in
  let o = Opcodes.tag_b tag i in
  let before_s = Simulator.make ~r ~pc () in
  let after_s = Simulator.step_lifted before_s o in
  test_last_op after_s o;
  return { x ; y ; o ; r ; src1 ; src2 ; imm ; before_s ; after_s }

let sprint_gen_b_vals { x ; y ; src1 ; src2 ; imm ; after_s ; _ } =
  Format.asprintf "X: %ld, y: %ld, Src1: %a, src2: %a, imm: %ld, after simulator: %a"
    x
    y
    Registers.pp_reg src1
    Registers.pp_reg src2
    imm
    Simulator.pp after_s

