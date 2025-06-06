
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
[@@deriving eq, sexp]
