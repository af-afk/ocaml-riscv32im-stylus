
(* Logical operations. From the variant field and the opcode field. *)
type t =
  (* ~~~ INTEGER REGISTER-IMMEDIATE INSTRUCTIONS ~~~ *)
  (* Extends the sign-extended 12-bit immediate to register rs1. *)
  | ADDI
  (**
   * Set less than immediate places the value 1 in register rd if register rs1 is less than
   * the sign-extended immediate when both are treated as signed numbers, else 0 is written to
   * rd.
  *)
  | SLTI
  (**
   * SLTIU compares the values as unsigned numbers (the immediate is first
   * sign-extended to XLEN bits, then treated as an unsigned number).
  *)
  | SLTIU
  (** Bitwise AND on register rs1 and the sign-extended 12 bit immediate and place the result
   * in rd. *)
  | ANDI
  (** Bitwise OR and the remainder of ANDI. *)
  | ORI
  (** Bitwise XOR and the remainder of ANDI. XORI rd, rs1, -1 performs a bitwise logical
   * inversion of register rs1. *)
  | XORI
  (** Logical left shift rs1 by the lower 5 bits of the I-immediate field. Right shift type is
   * encoded in bit 30. Zeroes are shifted into the lower bits. *)
  | SLLI
  (** Logical right shift like the previous. *)
  | SRLI
  (** Arithmetic right shift (the original sign bit is copied into the vacated upper bits. *)
  | SRAI
  (* ~~~ LUI/AUIPC INSTRUCTIONS ~~~ *)
  (**
   * Load upper intermediate is used to build 32-bit constants and uses the U type
   * format. The intermediate value is placed into the register rd, filling the lowest
   * 12 bits with zeroes.
  *)
  | LUI
  (*
   * Arithmetic right shift (the original sign bit is copied into the vacated upper bits.
   *)
  | AUIPC
  (* ~~~ INTEGER REGISTER-REGISTER INSTRUCTIONS ~~~ *)
  (**
   * Perform the addition of rs1 and rs2. Overflows are ignored, and XLEN bits of
   * results are written to the destination rd.
  *)
  | ADD
  (** Perform the subtraction of rs1 and rs2. Same overflow behaviour as ADD. *)
  | SUB
  (** Perform signed comparision. *)
  | SLT
  (**
   * Perform unsigned comparision. Writing 1 to rd if rs1 < rs2. Note that SLTU
   * rd, x0 rs2 sets rd to 1 if rs2 is not equal to 0. Otherwise sets rd to zero.
  *)
  | SLTU
  (** Bitwise AND. *)
  | AND
  (** Bitwise OR. *)
  | OR
  (** Bitwise XOR. *)
  | XOR
  | SLL
  | SRL
  | SRA
  (* ~~~ MULTIPLICATION INSTRUCTIONS ~~~ *)
  | MUL
  | MULH
  | MULHSU
  | MULHU
  | DIV
  | DIVU
  | REM
  | REMU
  (* ~~~ CONTROL TRANSFER INSTRUCTIONS ~~~ *)
  (**
   * JAL performs an unconditional jump, sign-extending the offset and
   * added to the address of the jump instruction to form the target
   * address. It stores the address of the instruction following the jump
   * (which is at program counter + 4) into register rd. The standard
   * calling convention uses x1 as the return address, and x5 as an
   * alternate link register.
  *)
  | JAL
  (**
   * JALR jump instruction obtains the target address for the JUMP
   * by adding the sign-extended 12-bit immediate to the register rs1,
   * then setting the least significant bit of the result to zero. The address
   * of the destination (which is at program counter + 4) is set to rd.
  *)
  | JALR
  (**
   * Take the branch if registers rs1 and rs2 are equal.
   *)
  | BEQ
  (**
   * Take the branch if registers r1 and rs2 are unequal.
   *)
  | BNE
  (**
   * Take the branch if rs1 is less than rs2 using signed comparision.
   *)
  | BLT
  (**
   * Take the branch if rs1 is less than rs2 using an unsigned operation.
   *)
  | BLTU
  (**
   * Take the branch if rs1 is greater than rs2 using a signed operation.
   *)
  | BGE
  (**
   * Take the branch if rs1 is greater than rs2 using an unsigned operation.
   *)
  | BGEU
  (** Load a 32-bit value from memory into rd. *)
  | LW
  (**
   * Load a 16-bit value from memory, then sign-extend it to 32 bits before
   * storing in rd.
  *)
  | LH
  (**
   * Load a 16-bit value from memory but then zero extend it to 32 bits
   * before storing it in rd.
  *)
  | LHU
  (**
   * Load a 8-bit value from memory, then sign extend it to 32 bits before
   * storing it in rd.
  *)
  | LB
  (**
   * Load a 8-bit from memory, but then zero extend it to 32 bits before
   * storing it in rd.
  *)
  | LBU
  (** Store the lower 32 bits of the register rs2 to memory. *)
  | SW
  (** Store the lower 16 bits of the register rs2 to memory. *)
  | SH
  (** Store the lower 8 bits of the register rs2 to memory. *)
  | SB
  (** Guarantee consistency of memory accesses. *)
  | FENCE
  (** Environment (system) call interface. Makes a call out with the ABI. *)
  | ECALL
  (** Cause a debugger break to the environment. *)
  | EBREAK
  (* ~~~ CONTROL AND STATUS REGISTERS  ~~~ *)
  | CSRRW
  | CSRRS
  | CSRRC
  | CSRRWI
  | CSRRSI
  | CSRRCI
[@@deriving eq, sexp, show, qcheck2]

(* ~~~ INTERMEDIATE OPERATIONS ~~~ *)

let mask_opcode_imm = 0x13

let mask_funct3_addi = 0
let mask_funct3_slti = 0x2
let mask_funct3_sltiu = 0x3
let mask_funct3_andi = 0x7
let mask_funct3_ori = 0x6
let mask_funct3_xori = 0x4
let mask_funct3_slli = 0x1

(* SRLI and SRAI differ in that to decode these two, funct7 must be consulted. *)
let mask_funct3_srli_and_srai = 0x5

let mask_funct7_srai = 0x20

let mask_opcode_lui = 0x37
let mask_opcode_auipc = 0x17

(* ~~~ INTEGER OPERATIONS ~~~ *)

let mask_opcode_op = 0x33

(* ADD and SUB need to be checked using funct7. *)
let mask_funct3_add_and_sub = 0
let mask_funct3_and = 0x7
let mask_funct3_or = 0x6
let mask_funct3_xor = 0x4
let mask_funct3_sll = 0x1
(* Use funct7 to decode this. *)
let mask_funct3_srl_and_sra = 0x5

let mask_funct7_sra = 0x20
let mask_funct7_sub = 0x20
let mask_funct7_mul = 0x1
let mask_funct7_mulh = 0x1
let mask_funct7_mulhsu = 01
let mask_funct7_mulhu = 0x1
let mask_funct7_div = 0x1
let mask_funct7_divu = 0x1
let mask_funct7_rem = 0x1
let mask_funct7_remu = 0x1

(* ~~~ CONTROL TRANSFER OPERATIONS ~~~ *)

let mask_opcode_jal = 0x6f
let mask_opcode_jalr = 0x67

let mask_opcode_branch = 0x63

let mask_funct3_beq = 0
let mask_funct3_bne = 0x1
let mask_funct3_blt = 0x4
let mask_funct3_bltu = 0x6
let mask_funct3_bge = 0x5
let mask_funct3_bgeu = 0x7

(* ~~~ LOAD AND STORE OPERATIONS ~~~ *)

let mask_opcode_load = 0x3
let mask_opcode_store = 0x23

let mask_funct3_lb_and_sb = 0
let mask_funct3_lh = 0x1
let mask_funct3_lhu = 0x5
let mask_funct3_lw = 0x2
let mask_funct3_lbu = 0x4
let mask_funct3_sw = 0x2
let mask_funct3_sh = 0x1

(* ~~~ MEMORY ORDERING INSTRUCTIONS ~~~ *)

let mask_opcode_fence = 0xf

(* ~~~ SYSTEM INSTRUCTIONS ~~~ *)

let mask_opcode_system = 0x73

let mask_funct12_ecall = 0
let mask_funct12_ebreak = 1

let mask_funct3_csrrw = 0x1
let mask_funct3_csrrs = 0x2
let mask_funct3_csrrc = 0x3
let mask_funct3_csrrwi = 0x5
let mask_funct3_csrrsi = 0x6
let mask_funct3_csrrci = 0x7
