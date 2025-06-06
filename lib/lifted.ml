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
  let r = { r_typ_dst = rd; r_typ_src1 = rs1; r_typ_src2 = rs2 } in
  let u = { u_typ_dst = rd; u_typ_imm = imm } in
  let s = { s_typ_src1 = rs1; s_typ_src2 = rs2; s_typ_imm = imm } in
  let j = { j_typ_dst = rd; j_typ_imm = imm } in
  let b = { b_typ_src1 = rs1 ; b_typ_src2 = rs2 ; b_typ_imm = imm } in
  match operation with
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

let pp_reg = Registers.pp_reg
let pp_reg_int = Registers.pp_reg

let pp_reg_int_maybe fmt x =
  Registers.pp_reg_int_maybe fmt (Int32.to_int x)

let pp_i fmt { i_typ_dst ; i_typ_src ; i_typ_imm } =
  Format.fprintf fmt "%a,%a,%a"
    pp_reg
    i_typ_dst
    pp_reg_int
    i_typ_src
    pp_reg_int_maybe
    i_typ_imm

let pp_r fmt { r_typ_dst ; r_typ_src1 ; r_typ_src2 } =
  Format.fprintf fmt "%a,%a,%a"
    pp_reg
    r_typ_dst
    pp_reg
    r_typ_src1
    pp_reg
    r_typ_src2

let pp_u fmt { u_typ_dst ; u_typ_imm } =
  Format.fprintf fmt "%a,%ld" pp_reg u_typ_dst u_typ_imm

let pp_s fmt { s_typ_src1 ; s_typ_src2 ; s_typ_imm } =
  Format.fprintf fmt "%a,%a,%a"
    pp_reg
    s_typ_src1
    pp_reg
    s_typ_src2
    pp_reg_int_maybe
    s_typ_imm

let pp_b fmt { b_typ_src1 ; b_typ_src2 ; b_typ_imm } =
  Format.fprintf fmt "%a,%a,%a"
    pp_reg
    b_typ_src1
    pp_reg
    b_typ_src2
    pp_reg_int_maybe
    b_typ_imm

let pp_j fmt { j_typ_dst ; j_typ_imm } =
  Format.fprintf fmt "%a,%a"
    pp_reg
    j_typ_dst
    pp_reg_int_maybe
    j_typ_imm

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
  | Slli i -> f "slli\t"; pp_i fmt i
  | Srli i -> f "srli\t"; pp_i fmt i
  | Srai i -> f "srai\t"; pp_i fmt i
  | Jalr i -> f "jalr\t"; pp_i fmt i
  | Lw i -> f "lw\t"; pp_i fmt i
  | Lh i -> f "lh\t"; pp_i fmt i
  | Lhu i -> f "lhu\t"; pp_i fmt i
  | Lb i -> f "lb\t"; pp_i fmt i
  | Lbu i -> f "lbu\t"; pp_i fmt i
  | Fence i -> f "fence\t"; pp_i fmt i
  | Ecall i -> f "ecall\t"; pp_i fmt i
  | Ebreak i -> f "ebreak\t"; pp_i fmt i
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
