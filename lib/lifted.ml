(**
 * Lifted type that can be interpreted literally or converted to a
 * abstract representation.
*)

open Sexplib0.Sexp_conv

type reg = Registers.reg

let pp_reg = Registers.pp_reg
let equal_reg = Registers.equal_reg
let reg_of_sexp = Registers.reg_of_sexp
let sexp_of_reg = Registers.sexp_of_reg

type i_typ =
  { i_typ_dst: reg
  ; i_typ_src: reg
  ; i_typ_imm: int32 }

and r_typ =
  { r_typ_dst: reg
  ; r_typ_src1: reg
  ; r_typ_src2: reg }

and u_typ = { u_typ_dst: reg; u_typ_imm: int32 }

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
  | Lui of u_typ
  | Auipc of u_typ
  | Jal of j_typ
  | Sw of s_typ
  | Sh of s_typ
  | Sb of s_typ
  | Mul of r_typ
  | Mulh of r_typ
  | Mulhsu of r_typ
  | Mulhu of r_typ
  | Div of r_typ
  | Divu of r_typ
  | Rem of r_typ
  | Remu of r_typ
  | Beq of b_typ
  | Bne of b_typ
  | Blt of b_typ
  | Bltu of b_typ
  | Bge of b_typ
  | Bgeu of b_typ
[@@deriving show, eq, sexp]

let from_word w =
  let open Decoding in
  let { t_operation; t_rd; t_rs1; t_rs2; t_imm } = Decoding.from w in
  let rd = Registers.of_int t_rd in
  let rs1 = Registers.of_int t_rs1 in
  let rs2 = Registers.of_int t_rs2 in
  let i = { i_typ_dst = rd; i_typ_src = rs1; i_typ_imm = t_imm } in
  let r = { r_typ_dst = rd; r_typ_src1 = rs1; r_typ_src2 = rs2 } in
  let u = { u_typ_dst = rd; u_typ_imm = t_imm } in
  let s = { s_typ_src1 = rs1; s_typ_src2 = rs2; s_typ_imm = t_imm } in
  let j = { j_typ_dst = rd; j_typ_imm = t_imm } in
  let b = { b_typ_src1 = rs1 ; b_typ_src2 = rs2 ; b_typ_imm = t_imm } in
  match t_operation with
  (* I-type instructions *)
  | ADDI -> Addi i
  | SLTI -> Slti i
  | SLTIU -> Sltiu i
  | ANDI -> Andi i
  | ORI -> Ori i
  | XORI -> Xori i
  | SLLI -> Slli i
  | SRLI -> Srli i
  | SRAI -> Srai i
  | JALR -> Jalr i
  | LW -> Lw i
  | LH -> Lh i
  | LHU -> Lhu i
  | LB -> Lb i
  | LBU -> Lbu i
  | FENCE -> Fence i
  | ECALL -> Ecall i
  | EBREAK -> Ebreak i
  (* R-type instructions *)
  | ADD -> Add r
  | SUB -> Sub r
  | SLT -> Slt r
  | SLTU -> Sltu r
  | AND -> And r
  | OR -> Or r
  | XOR -> Xor r
  | SLL -> Sll r
  | SRL -> Srl r
  | SRA -> Sra r
  (* U-type instructions *)
  | LUI -> Lui u
  | AUIPC -> Auipc u
  (* J-type instruction *)
  | JAL -> Jal j
  (* S-type instructions *)
  | SW -> Sw s
  | SH -> Sh s
  | SB -> Sb s
  (* B-type instructions *)
  | BEQ -> Beq b
  | BNE -> Bne b
  | BLT -> Blt b
  | BLTU -> Bltu b
  | BGE -> Bge b
  | BGEU -> Bgeu b
