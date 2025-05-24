
[@@@ocaml.warning "+A"]

module B = Fmlib_std.Btree.Map (Int)

module Registers = struct
  (*
   * Registers that the machine supports. Named after the convention I observed Spike
   * using.
   *)
  type t =
    { t_r_zero: int (* Always zero! *)
    ; t_r_ra: int (* For function calls *)
    ; t_r_sp: int (* Stack pointer *)
    ; t_r_gp: int (* Global variables pointer *)
    ; t_r_tp: int (* Thread pointer *)
    (* Temporary registers *)
    ; t_r_t0: int ; t_r_t1: int ; t_r_t2: int
    ; t_r_t3: int ; t_r_t4: int
    ; t_r_t5: int ; t_r_t6: int
    (* Saved registers *)
    ; t_r_s0: int ; t_r_s1: int
    ; t_r_s2: int ; t_r_s3: int ; t_r_s4: int
    ; t_r_s5: int ; t_r_s6: int ; t_r_s7: int
    ; t_r_s8: int ; t_r_s9: int ; t_r_s10: int
    ; t_r_s11: int
    (* Argument/return value registers *)
    ; t_r_a0: int ; t_r_a1: int
    ; t_r_a2: int ; t_r_a3: int ; t_r_a4: int
    ; t_r_a5: int ; t_r_a6: int
    ; t_r_a7: int (* Also the ECALL number *)
    }
  [@@deriving show, eq]

  let empty =
    { t_r_zero = 0; t_r_ra = 0; t_r_sp = 0; t_r_gp = 0; t_r_tp = 0
    ; t_r_t0 = 0; t_r_t1 = 0; t_r_t2 = 0; t_r_s0 = 0; t_r_s1 = 0; t_r_a0 = 0
    ; t_r_a1 = 0; t_r_a2 = 0; t_r_a3 = 0; t_r_a4 = 0; t_r_a5 = 0; t_r_a6 = 0
    ; t_r_a7 = 0; t_r_s2 = 0; t_r_s3 = 0; t_r_s4 = 0; t_r_s5 = 0; t_r_s6 = 0
    ; t_r_s7 = 0; t_r_s8 = 0; t_r_s9 = 0; t_r_s10 = 0; t_r_s11 = 0; t_r_t3 = 0
    ; t_r_t4 = 0; t_r_t5 = 0; t_r_t6 = 0 }

  type reg =
    [ `Zero (* Always zero! *)
    | `Ra   (* Return address *)
    | `Sp   (* Stack pointer *)
    | `Gp   (* Global pointer *)
    | `Tp   (* Thread pointer *)
    (* Temporaries *)
    | `T0 | `T1 | `T2 | `T3 | `T4 | `T5 | `T6
    (* Saved registers *)
    | `S0 | `S1 | `S2 | `S3 | `S4 | `S5 | `S6 | `S7 | `S8 | `S9 | `S10 | `S11
    (* Argument/return registers *)
    | `A0 | `A1 | `A2 | `A3 | `A4 | `A5 | `A6
    | `A7  (* A7 also ECALL number *) ]
  [@@deriving show, eq, sexp]

  let from_bitv = function
    |  0l -> `Zero |  1l -> `Ra   |  2l -> `Sp
    |  3l -> `Gp   |  4l -> `Tp   |  5l -> `T0
    |  6l -> `T1   |  7l -> `T2   |  8l -> `S0
    |  9l -> `S1   | 10l -> `A0   | 11l -> `A1
    | 12l -> `A2   | 13l -> `A3   | 14l -> `A4
    | 15l -> `A5   | 16l -> `A6   | 17l -> `A7
    | 18l -> `S2   | 19l -> `S3   | 20l -> `S4
    | 21l -> `S5   | 22l -> `S6   | 23l -> `S7
    | 24l -> `S8   | 25l -> `S9   | 26l -> `S10
    | 27l -> `S11  | 28l -> `T3   | 29l -> `T4
    | 30l -> `T5   | 31l -> `T6   | _ -> invalid_arg "unknown register"
end

(*
 * Low level representation of the operations and the details. Opcodes are converted to a
 * type and the rest of the word is encoded as a bitvec, then cut up according to the
 * lengths.
 *)
module Decoding = struct
  (* Logical operations. From the variant field and the opcode field. *)
  type operation =
    (* ~~~ INTEGER REGISTER-IMMEDIATE INSTRUCTIONS ~~~ *)
    (** Extends the sign-extended 12-bit immediate to register rs1. *)
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
  [@@deriving eq, sexp]

  (*
   * Decoded word in our high semi-high representation. We do not include the opcode,
   * funct3 or funct7 here! These fields are turned into the operation.
   *)
  type t =
    { t_operation: operation
    (** Destination register. *)
    ; t_rd: int
    (** The first register in an instruction. *)
    ; t_rs1: int
    (** The second register in an instruction. *)
    ; t_rs2: int
    (** The immediate literal embedded in the instruction. Variably sized. *)
    ; t_imm: int32 }
  [@@deriving eq, make]

  (* Decoded word higher level representation. *)
  let empty =
    { t_operation = ADDI
    ; t_rd = 0
    ; t_rs1 = 0
    ; t_rs2 = 0
    ; t_imm = 0l }

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
  let mask_funct3_slti = 0x2
  let mask_funct3_and = 0x7
  let mask_funct3_or = 0x6
  let mask_funct3_xor = 0x4
  let mask_funct3_sll = 0x1
  (* Use funct7 to decode this. *)
  let mask_funct3_srl_and_sra = 0x5

  let mask_funct7_sra = 0x20
  let mask_funct7_sub = 0x20

  (* ~~~ CONTROL TRANSFER OPERATIONS ~~~ *)

  let mask_opcode_jal = 0x6f
  let mask_opcode_jalr = 0x67

  let mask x = (1 lsl x) - 1

  let unpack_field w i l = (w lsr i) land (mask l)

  let unpack_operation w =
    let op = unpack_field w 0 7 in
    let funct3 = unpack_field w 12 3 in
    let funct7 = unpack_field w 25 7 in
    match op with
    | op when op = mask_opcode_imm -> (* OP-IMM *)
      (match funct3 with
       | f when f = mask_funct3_addi -> ADDI
       | f when f = mask_funct3_slti -> SLTI
       | f when f = mask_funct3_sltiu -> SLTIU
       | f when f = mask_funct3_andi -> ANDI
       | f when f = mask_funct3_ori  -> ORI
       | f when f = mask_funct3_xori -> XORI
       | f when f = mask_funct3_slli -> SLLI
       | f when f = mask_funct3_srli_and_srai && funct7 = mask_funct7_srai -> SRAI
       | f when f = mask_funct3_srli_and_srai -> SRLI
       | _ -> invalid_arg "unknown IMM variant"
      )
    | op when op = mask_opcode_op -> (* OP *)
      (match funct3 with
       | f when f = mask_funct3_add_and_sub && funct7 = mask_funct7_sub -> SUB
       | f when f = mask_funct3_add_and_sub -> ADD
       | f when f = mask_funct3_sll -> SLL
       | f when f = mask_funct3_srl_and_sra && funct7 = mask_funct7_sra -> SRA
       | f when f = mask_funct3_srl_and_sra -> SRL
       | f when f = mask_funct3_slti -> SLT
       | f when f = mask_funct3_sltiu -> SLTU
       | f when f = mask_funct3_and -> AND
       | f when f = mask_funct3_or -> OR
       | f when f = mask_funct3_xor -> XOR
       | _ -> invalid_arg "unknown OP variant"
      )
    | op when op = mask_opcode_lui -> LUI
    | op when op = mask_opcode_auipc -> AUIPC
    | op when op = mask_opcode_jal -> JAL
    | op when op = mask_opcode_jalr -> JALR
    | _ -> invalid_arg "unknown opcode"
    | _ -> invalid_arg "unknown opcode"

  let from w =
    let op = unpack_operation w in
    let t = { empty with t_operation = op } in
    match op with
    (* I-type instructions *)
    | ADDI | SLTI | SLTIU | ANDI | ORI | XORI ->
      { t with
        t_operation = op
      ; t_rd = unpack_field w 7 5
      ; t_rs1 = unpack_field w 15 5
      ; t_imm = Int32.of_int (unpack_field w 20 12) }
    | SLLI | SRLI | SRAI ->
      { t with
        t_operation = op
      ; t_rd = unpack_field w 7 5
      ; t_rs1 = unpack_field w 15 5
      ; t_imm = Int32.of_int (unpack_field w 20 5) }
    (* U-type instructions *)
    | LUI | AUIPC ->
      { t with
        t_operation = op
      ; t_rd = unpack_field w 7 5
      ; t_imm = unpack_field w 12 20 }
    (* J-type instruction *)
    | JAL ->
      { t with
      t _operation = op
    ; t_rd = unpack_field w 7 5
    ; t_imm = unpack_field 12 80
    ;
end

(*
 * Lifted type that can be interpreted literally or converted to a
 * abstract representation.
 *)
module Lifted = struct
  type t =
    | Addi of [`Dest of Registers.reg] * [`Src of Registers.reg] * int
    | Slti of [`Dest of Registers.reg] * [`Src of Registers.reg] * int
    | Andi of [`Dest of Registers.reg] * [`Src of Registers.reg] * int
    | Ori of [`Dest of Registers.reg] * [`Src of Registers.reg] * int
    | Xori of [`Dest of Registers.reg] * [`Src of Registers.reg] * int
    | Slli of [`Dest of Registers.reg] * [`Src of Registers.reg] * int
    | Srli of [`Dest of Registers.reg] * [`Src of Registers.reg] * int
    | Srai of [`Dest of Registers.reg] * [`Src of Registers.reg] * int
    | Lui of [`Dest of Registers.reg] * int
  [@@deriving eq]

  let from_word w =
    let open Decoding in
    let { t_operation; t_rd; t_rs1; t_rs2; t_imm } = Decoding.from w in
    let t_rd = Registers.from_bitv t_rd in
    let t_rs1 = Registers.from_bitv t_rs1 in
    match t_operation with
    | ADDI -> Addi (`Dest t_rd, `Src t_rs1, t_imm)
    | SLTI -> Slti (`Dest t_rd, `Src t_rs1, t_imm)
    | ANDI -> Andi (`Dest t_rd, `Src t_rs1, t_imm)
    | ORI -> Ori (`Dest t_rd, `Src t_rs1, t_imm)
    | XORI -> Xori (`Dest t_rd, `Src t_rs1, t_imm)
    | SLLI -> Slli (`Dest t_rd, `Src t_rs1, t_imm)
    | SRLI -> Srli (`Dest t_rd, `Src t_rs1, t_imm)
    | SRAI -> Srai (`Dest t_rd, `Src t_rs1, t_imm)
    | LUI -> Lui (`Dest t_rd, t_imm)
end

(*
 * Bringing it all together, we need to interpret the Lifted
 * representation using the Registers, and the B interface for our block
 * storage.
 *)
module Interpreter = struct
  type t =
    { r: Registers.t
    ; b: int B.t }

  let step_addi t dst src imm = ()

  let step t = Lifted.(function
      | Addi (`Dest dst, `Src src, imm) -> step_addi t dst src
    )
end
