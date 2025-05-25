(**
 * Lifted type that can be interpreted literally or converted to a
 * abstract representation.
 *)

type reg = Registers.reg

type i_typ =
  { i_typ_dst: reg
  ; i_typ_src: reg
  ; i_typ_imm: int32 }

and r_typ =
  { r_typ_dst: reg
  ; r_typ_src1: reg
  ; r_typ_src2: reg }

and u_type = { u_typ_dst: reg; u_typ_imm: int32 }

and s_typ =
  { s_typ_src1: reg
  ; s_typ_src2: reg
  ; s_typ_imm: int32 }

and b_typ =
  { b_typ_src1: reg
  ; b_typ_src2: reg
  ; b_typ_imm: int32 }

and j_typ = { j_typ_dst: reg ; j_typ_imm: int32 }

and t =
  | Addi of i_typ
  | Slti of i_typ
  | Sltiu of i_typ
  | Andi of i_typ
  | Ori of i_typ
  | Xori of i_typ
  | Jalr of i_typ
  | Lh of i_typ
  | Lhu of i_typ
  | Lb of i_typ
  | Lbu of i_typ
  | Lw of i_typ
  | Slli of i_typ
  | Srli of i_typ
  | Srai of i_typ
  | Ecall of i_typ
  | Ebreak of i_typ
  | Fence of i_typ
  | Add of r_typ
  | Sub of r_typ
  | Slt of r_typ
  | Sltu of r_typ
  | And of r_typ
  | Or of r_typ
  | Xor of r_typ
  | Sll of r_typ
  | Srl of r_typ
  | Sra of r_typ
[@@deriving show, eq, sexp]

let from_word w =
  let open Decoding in
  let { t_operation; t_rd; t_rs1; t_rs2; t_imm } = Decoding.from w in
  let _ = t_rs2 in
  let rd = Registers.of_int t_rd in
  let rs1 = Registers.of_int t_rs1 in
  let i = { i_typ_dst = rd; i_typ_src = rs1; i_typ_imm = t_imm } in
  match t_operation with
  | ADDI -> Addi i
  | _ -> failwith ""
