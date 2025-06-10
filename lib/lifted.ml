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
let gen_reg = Registers.gen_reg

(*
 * We need these types so that we can generate differently packaged
 * ranges for the test suite.
 *)
type imm = int32
and imm_i_typ_shift = imm
and imm_u_typ = imm
and imm_j_typ = imm
and imm_b_typ = imm
[@@deriving show, eq, sexp]

let gen_imm =
  QCheck2.Gen.(int_range (-2048) 2047 |> map Int32.of_int)

let gen_imm_i_typ_shift =
  QCheck2.Gen.(int_range 0 31 |> map Int32.of_int)

let gen_imm_u_typ =
  let open QCheck2.Gen in
  let* upper_20_bits = int_range (-524288) 524287 in  (* 20-bit signed range *)
  return (Int32.shift_left (Int32.of_int upper_20_bits) 12)

let gen_imm_j_typ =
  (*
   * There is an issue with the shrinking function taking us past the
   * limit. So this takes half the valid range, then multiplies it by two
   * so we never go over!
   *)
  let open QCheck2.Gen in
  let* offset = int_range (-524288) 524287 in
  return (Int32.of_int (offset * 2))

let gen_imm_b_typ =
  (* 12-bit signed range, even only*)
  let open QCheck2.Gen in
  let* offset = int_range (-2048) 2047 in
  return (Int32.of_int (offset * 2))

type i_typ_sys =
  { i_typ_sys_dst: reg
  ; i_typ_sys_src: reg
  ; i_typ_sys_imm: imm }
[@@deriving show, eq, sexp]

let empty_i_typ_sys =
  { i_typ_sys_dst = `Zero
  ; i_typ_sys_src = `Zero
  ; i_typ_sys_imm = 0l }

let gen_i_typ_sys =
  QCheck2.Gen.return empty_i_typ_sys

type i_typ =
  { i_typ_dst: reg
  ; i_typ_src: reg
  ; i_typ_imm: imm }

and i_typ_sft =
  { i_typ_sft_dst: reg
  ; i_typ_sft_src: reg
  ; i_typ_sft_imm: imm_i_typ_shift }

and u_typ = { u_typ_dst: reg; u_typ_imm: imm_u_typ }

and r_typ =
  { r_typ_dst: reg
  ; r_typ_src1: reg
  ; r_typ_src2: reg }

and s_typ =
  { s_typ_src1: reg
  ; s_typ_src2: reg
  ; s_typ_imm: imm }

and b_typ =
  { b_typ_src1: reg
  ; b_typ_src2: reg
  ; b_typ_imm: imm_b_typ }

and j_typ = { j_typ_dst: reg ; j_typ_imm: imm_j_typ }

and t =
  | Addi of i_typ | Slti of i_typ | Sltiu of i_typ | Andi of i_typ
  | Ori of i_typ | Xori of i_typ | Jalr of i_typ | Lh of i_typ
  | Lhu of i_typ | Lb of i_typ | Lbu of i_typ | Lw of i_typ
  | Slli of i_typ_sft | Srli of i_typ_sft | Srai of i_typ_sft | Ecall of i_typ_sys
  | Ebreak of i_typ_sys | Fence of i_typ
  | Add of r_typ | Sub of r_typ | Slt of r_typ | Sltu of r_typ
  | And of r_typ | Or of r_typ | Xor of r_typ | Sll of r_typ
  | Srl of r_typ | Sra of r_typ | Mul of r_typ | Mulh of r_typ
  | Mulhsu of r_typ | Mulhu of r_typ | Div of r_typ | Divu of r_typ
  | Rem of r_typ | Remu of r_typ
  | Lui of u_typ | Auipc of u_typ
  | Jal of j_typ
  | Sw of s_typ | Sh of s_typ | Sb of s_typ
  | Beq of b_typ | Bne of b_typ | Blt of b_typ | Bltu of b_typ
  | Bge of b_typ | Bgeu of b_typ
[@@deriving show, eq, sexp, qcheck2]

let empty_i_type =
  { i_typ_dst = `Zero
  ; i_typ_src = `Zero
  ; i_typ_imm = 0l }

let get_opcode_mask =
  let open Operation in
  function
  (* IMM *)
  | Addi _ | Slti _ | Sltiu _ | Andi _
  | Ori _ | Xori _ | Slli _ | Srli _ | Srai _ -> mask_opcode_imm
  | Ecall _ | Ebreak _ -> mask_opcode_system
  | Fence _ -> mask_opcode_fence
  (* LALR *)
  | Jalr _ -> mask_opcode_jalr
  (* LOAD *)
  | Lh _ | Lhu _ | Lb _ | Lbu _
  | Lw _ -> mask_opcode_load
  (* Arithmetic/Logic *)
  | Add _ | Sub _ | Slt _ | Sltu _
  | And _ | Or _ | Xor _ | Sll _
  | Srl _ | Sra _ | Mul _ | Mulh _
  | Mulhsu _ | Mulhu _ | Div _ | Divu _
  | Rem _ | Remu _ -> mask_opcode_op
  (* Upper immediate *)
  | Lui _ -> mask_opcode_lui
  (* Add upper immediate to PC *)
  | Auipc _ -> mask_opcode_auipc
  (* Jump and link *)
  | Jal _ -> mask_opcode_jal
  (* Store operations *)
  | Sw _ | Sh _ | Sb _ -> mask_opcode_store
  (* Branch operations *)
  | Beq _ | Bne _ | Blt _ | Bltu _
  | Bge _ | Bgeu _ -> mask_opcode_branch

let get_funct3_mask =
  let open Operation in
  function
  | Addi _ -> mask_funct3_addi
  | Slti _ -> mask_funct3_slti
  | Sltiu _ -> mask_funct3_sltiu
  | Andi _ -> mask_funct3_andi
  | Ori _ -> mask_funct3_ori
  | Xori _ -> mask_funct3_xori
  | Jalr _ -> 0
  | Lh _ -> mask_funct3_lh
  | Lhu _ -> mask_funct3_lhu
  | Lb _ -> mask_funct3_lb_and_sb
  | Lbu _ -> mask_funct3_lbu
  | Lw _ -> mask_funct3_lw
  | Slli _ -> mask_funct3_slli
  | Srli _ -> mask_funct3_srli_and_srai
  | Srai _ -> mask_funct3_srli_and_srai
  | Ecall _ -> 0
  | Ebreak _ -> 0
  | Fence _ -> 0
  | Add _ -> mask_funct3_add_and_sub
  | Sub _ -> mask_funct3_add_and_sub
  | Slt _ -> mask_funct3_slti
  | Sltu _ -> mask_funct3_sltiu
  | And _ -> mask_funct3_and
  | Or _ -> mask_funct3_or
  | Xor _ -> mask_funct3_xor
  | Sll _ -> mask_funct3_sll
  | Srl _ -> mask_funct3_srl_and_sra
  | Sra _ -> mask_funct3_srl_and_sra
  | Lui _ -> 0  (* U-type instructions don't have funct3 *)
  | Auipc _ -> 0  (* U-type instructions don't have funct3 *)
  | Jal _ -> 0  (* J-type instructions don't have funct3 *)
  | Sw _ -> mask_funct3_sw
  | Sh _ -> mask_funct3_sh
  | Sb _ -> mask_funct3_lb_and_sb
  | Mul _ -> 0
  | Mulh _ -> 0x1
  | Mulhsu _ -> 0x2
  | Mulhu _ -> 0x3
  | Div _ -> 0x4
  | Divu _ -> 0x5
  | Rem _ -> 0x6
  | Remu _ -> 0x7
  | Beq _ -> mask_funct3_beq
  | Bne _ -> mask_funct3_bne
  | Blt _ -> mask_funct3_blt
  | Bltu _ -> mask_funct3_bltu
  | Bge _ -> mask_funct3_bge
  | Bgeu _ -> mask_funct3_bgeu

let get_funct7_mask =
  let open Operation in
  function
  | Srai _ -> Some mask_funct7_srai
  | Sub _ -> Some mask_funct7_sub
  | Sra _ -> Some mask_funct7_sra
  | Mul _ | Mulh _ | Mulhsu _ | Div _ | Divu _ | Rem _ | Remu _-> Some 0x1
  | _ -> None

let get_funct12_mask =
  let open Operation in
  function
  | Ecall _ -> Some mask_funct12_ecall
  | Ebreak _ -> Some mask_funct12_ebreak
  | _ -> None

let empty_i =
  { i_typ_dst = Registers.zero
  ; i_typ_src= Registers.zero
  ; i_typ_imm = 0l }

let empty_r =
  { r_typ_dst = Registers.zero
  ; r_typ_src1 = Registers.zero
  ; r_typ_src2 = Registers.zero }

let empty_u =
  { u_typ_dst = Registers.zero
  ; u_typ_imm = 0l }

let empty_s =
  { s_typ_src1 = Registers.zero
  ; s_typ_src2 = Registers.zero
  ; s_typ_imm = 0l }

let empty_b =
  { b_typ_src1 = Registers.zero
  ; b_typ_src2 = Registers.zero
  ; b_typ_imm = 0l }

let empty_j =
  { j_typ_dst = Registers.zero
  ; j_typ_imm = 0l }

let empty = Addi empty_i

let from_word loc w =
  let open Decoding in
  let { operation; rd; rs1; rs2; imm; _ } = Decoding.from loc w in
  let rd = Registers.of_int rd in
  let rs1 = Registers.of_int rs1 in
  let rs2 = Registers.of_int rs2 in
  let i = { i_typ_dst = rd; i_typ_src = rs1; i_typ_imm = imm } in
  let i_sft = { i_typ_sft_dst = rd; i_typ_sft_src = rs1; i_typ_sft_imm = imm } in
  let i_sys = { i_typ_sys_dst = rd; i_typ_sys_src = rs1; i_typ_sys_imm = imm } in
  let r = { r_typ_dst = rd; r_typ_src1 = rs1; r_typ_src2 = rs2 } in
  let u = { u_typ_dst = rd; u_typ_imm = imm } in
  let s = { s_typ_src1 = rs1; s_typ_src2 = rs2; s_typ_imm = imm } in
  let j = { j_typ_dst = rd; j_typ_imm = imm } in
  let b = { b_typ_src1 = rs1 ; b_typ_src2 = rs2 ; b_typ_imm = imm } in
  match operation with
  (* I-type *)
  | ADDI -> Addi i | SLTI -> Slti i | SLTIU -> Sltiu i | ANDI -> Andi i
  | ORI -> Ori i | XORI -> Xori i | SLLI -> Slli i_sft | SRLI -> Srli i_sft
  | SRAI -> Srai i_sft | JALR -> Jalr i | LW -> Lw i | LH -> Lh i
  | LHU -> Lhu i | LB -> Lb i | LBU -> Lbu i | FENCE -> Fence i
  | ECALL -> Ecall i_sys | EBREAK -> Ebreak i_sys
  (* R-type *)
  | ADD -> Add r | SUB -> Sub r | SLT -> Slt r | SLTU -> Sltu r
  | AND -> And r | OR -> Or r | XOR -> Xor r | SLL -> Sll r
  | SRL -> Srl r | SRA -> Sra r | MUL -> Mul r | MULH -> Mulh r
  | MULHSU -> Mulhsu r | MULHU -> Mulhu r | DIV -> Div r
  | DIVU -> Divu r | REM -> Rem r | REMU -> Remu r
  (* U-type *)
  | LUI -> Lui u | AUIPC -> Auipc u
  (* J-type *)
  | JAL -> Jal j
  (* S-type *)
  | SW -> Sw s | SH -> Sh s | SB -> Sb s
  (* B-type *)
  | BEQ -> Beq b | BNE -> Bne b | BLT -> Blt b | BLTU -> Bltu b
  | BGE -> Bge b | BGEU -> Bgeu b

let pp_reg = Registers.pp_reg
let pp_reg_int = Registers.pp_reg

let pp_reg_int_maybe fmt x =
  Registers.pp_reg_int_maybe fmt (Int32.to_int x)

let pp_i fmt { i_typ_dst ; i_typ_src ; i_typ_imm } =
  Format.fprintf fmt "%a,%ld(%a)"
    pp_reg i_typ_dst
    i_typ_imm
    pp_reg_int i_typ_src

let pp_i_sft fmt { i_typ_sft_dst ; i_typ_sft_src; i_typ_sft_imm } =
  Format.fprintf fmt "%a,%ld(%a)"
    pp_reg i_typ_sft_dst
    i_typ_sft_imm
    pp_reg_int i_typ_sft_src

let pp_i_sys fmt { i_typ_sys_dst ; i_typ_sys_src; i_typ_sys_imm } =
  Format.fprintf fmt "%a,%ld(%a)"
    pp_reg i_typ_sys_dst
    i_typ_sys_imm
    pp_reg_int i_typ_sys_src

let pp_r fmt { r_typ_dst ; r_typ_src1 ; r_typ_src2 } =
  Format.fprintf fmt "%a,%a,%a"
    pp_reg r_typ_dst
    pp_reg r_typ_src1
    pp_reg r_typ_src2

let pp_u fmt { u_typ_dst ; u_typ_imm } =
  Format.fprintf fmt "%a,%ld" pp_reg u_typ_dst u_typ_imm

let pp_s fmt { s_typ_src1 ; s_typ_src2 ; s_typ_imm } =
  Format.fprintf fmt "%a,%ld(%a)"
    pp_reg s_typ_src2
    s_typ_imm
    pp_reg s_typ_src1

let pp_b fmt { b_typ_src1 ; b_typ_src2 ; b_typ_imm } =
  Format.fprintf fmt "%a,%a,%a"
    pp_reg b_typ_src1
    pp_reg b_typ_src2
    pp_reg_int_maybe b_typ_imm

let pp_j fmt { j_typ_dst ; j_typ_imm } =
  Format.fprintf fmt "%a,%a"
    pp_reg j_typ_dst
    pp_reg_int_maybe j_typ_imm

let pp_objdump fmt t =
  let f s = Format.fprintf fmt s in
  match t with
  (* I-type instructions *)
  | Addi i -> f "addi\t"; pp_i fmt i
  | Slti i -> f "slti\t"; pp_i fmt i
  | Sltiu i -> f "sltiu\t"; pp_i fmt i
  | Andi i -> f "andi\t"; pp_i fmt i
  | Ori i -> f "ori\t"; pp_i fmt i
  | Xori i -> f "xori\t"; pp_i fmt i
  | Slli i -> f "slli\t"; pp_i_sft fmt i
  | Srli i -> f "srli\t"; pp_i_sft fmt i
  | Srai i -> f "srai\t"; pp_i_sft fmt i
  | Jalr i -> f "jalr\t"; pp_i fmt i
  | Lw i -> f "lw\t"; pp_i fmt i
  | Lh i -> f "lh\t"; pp_i fmt i
  | Lhu i -> f "lhu\t"; pp_i fmt i
  | Lb i -> f "lb\t"; pp_i fmt i
  | Lbu i -> f "lbu\t"; pp_i fmt i
  | Fence i -> f "fence\t"; pp_i fmt i
  | Ecall i -> f "ecall\t"; pp_i_sys fmt i
  | Ebreak i -> f "ebreak\t"; pp_i_sys fmt i
  (* R-type instructions *)
  | Add r -> f "add\t"; pp_r fmt r
  | Sub r -> f "sub\t"; pp_r fmt r
  | Slt r -> f "slt\t"; pp_r fmt r
  | Sltu r -> f "sltu\t"; pp_r fmt r
  | And r -> f "and\t"; pp_r fmt r
  | Or r -> f "or\t"; pp_r fmt r
  | Xor r -> f "xor\t"; pp_r fmt r
  | Sll r -> f "sll\t"; pp_r fmt r
  | Srl r -> f "srl\t"; pp_r fmt r
  | Sra r -> f "sra\t"; pp_r fmt r
  (* M-extension instructions (R-type) *)
  | Mul r -> f "mul\t"; pp_r fmt r
  | Mulh r -> f "mulh\t"; pp_r fmt r
  | Mulhsu r -> f "mulhsu\t"; pp_r fmt r
  | Mulhu r -> f "mulhu\t"; pp_r fmt r
  | Div r -> f "div\t"; pp_r fmt r
  | Divu r -> f "divu\t"; pp_r fmt r
  | Rem r -> f "rem\t"; pp_r fmt r
  | Remu r -> f "remu\t"; pp_r fmt r
  (* U-type instructions *)
  | Lui u -> f "lui\t"; pp_u fmt u
  | Auipc u -> f "auipc\t"; pp_u fmt u
  (* J-type instruction *)
  | Jal j -> f "jal\t"; pp_j fmt j
  (* S-type instructions *)
  | Sw s -> f "sw\t"; pp_s fmt s
  | Sh s -> f "sh\t"; pp_s fmt s
  | Sb s -> f "sb\t"; pp_s fmt s
  (* B-type instructions *)
  | Beq b -> f "beq\t"; pp_b fmt b
  | Bne b -> f "bne\t"; pp_b fmt b
  | Blt b -> f "blt\t"; pp_b fmt b
  | Bltu b -> f "bltu\t"; pp_b fmt b
  | Bge b -> f "bge\t"; pp_b fmt b
  | Bgeu b -> f "bgeu\t"; pp_b fmt b
