
(*
 * Bringing it all together, we need to interpret the Lifted
 * representation using the Registers, and the B interface for our block
 * storage.
 *)

module B = Fmlib_std.Btree.Map (Int)

type t = { r: Registers.t ; b: int B.t }

let (+) = Int32.add

let sign_extend_12 x =
  let open Int32 in
  let x = logand x 0xfffl in
  if logand x 0x800l <> 0l then
    logor x (lognot 0xfffl)
  else
    x

let step_addi t dst src imm =
  let { r ; _ } = t in
  let imm = sign_extend_12 imm in
  { t with r = Registers.update r dst ((Registers.get r src) + imm) }

let step_slti t dst src imm =
  let { r ; _ } = t in
  let imm = sign_extend_12 imm in
  { t with r = Registers.update r dst (
        if Registers.get r src > imm then 1l else 0l
      ) }

let step_sltiu t dst src imm =
  let { r ; _ } = t in
  let imm = sign_extend_12 imm in
  { t with r = Registers.update r dst (
        if Int32.unsigned_compare (Registers.get r src) imm > 0 then 1l else 0l
      ) }

let step_andi t dst src imm =
  let { r ; _ } = t in
  let imm = sign_extend_12 imm in
  { t with r = Registers.update r dst (Int32.logand (Registers.get r src) imm) }

let step_ori t dst src imm =
  let { r ; _ } = t in
  let imm = sign_extend_12 imm in
  { t with r = Registers.update r dst (Int32.logor (Registers.get r src) imm) }

let step_xori t dst src imm =
  let { r ; _ } = t in
  let imm = sign_extend_12 imm in
  { t with r = Registers.update r dst (Int32.logxor (Registers.get r src) imm) }

let step t f =
  let open Lifted in
  let apply_i {  i_typ_dst; i_typ_src; i_typ_imm } f =
    (* For the I-type instructions, rd is the dst, and rs1 is the src. *)
    f t i_typ_dst i_typ_src i_typ_imm in
  match f with
  | Addi f -> apply_i f step_addi
  | Slti f -> apply_i f step_slti
  | Sltiu f -> apply_i f step_sltiu
  | Andi f -> apply_i f step_andi
  | Ori f -> apply_i f step_ori
  | Xori f -> apply_i f step_xori
  | _ -> invalid_arg "TODO"

let apply = Seq.fold_left step
