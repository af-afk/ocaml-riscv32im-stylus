
open QCheck2.Gen

open Riscv32im_stylus

let gen_random_memory_access m =
  let* m = m in
  let min_addr = List.fold_left (fun acc Memory.Region.{ base; _ } ->
      min acc base) Int.max_int m in
  let max_addr = List.fold_left (fun acc Memory.Region.{ base; size; _ } ->
      max acc (base + size - 1)) Int.min_int m in
  let* addr = int_range min_addr (max_addr - 3) in
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
  ; res: int32 }

let gen_i_registers_pair imm_f tag_f tag =
  let open QCheck2.Gen in
  let* x = int32 in
  let* y = imm_f in
  let* src = Registers.gen_reg_nonzero in
  let* dst = Registers.gen_reg_nonzero in
  let i = Lifted.{ i_typ_dst = dst ; i_typ_src = src ; i_typ_imm = y } in
  let o = tag_f tag i in
  let before_s = Simulator.make ~r: Registers.(update empty src x) () in
  let after_s = Simulator.(step_lifted empty_fmt before_s o) in
  let res = Registers.get after_s.r dst in
  test_last_op after_s o;
  return { o ; r = after_s.r ; src ; dst ; x ; y ; before_s; after_s; res }

let gen_i_registers_and_values x =
  gen_i_registers_pair Lifted.gen_imm Opcodes.tag_i x

let sprint_gen_i_registers_vals { src ; dst ; x ; y ; after_s ; res ;_ } =
  Format.asprintf "Val1: %ld, val2: %ld, src: %a, dst: %a, after simulator: %a, result: %lx"
    x
    y
    Registers.pp_reg src
    Registers.pp_reg dst
    Simulator.pp after_s
    res
