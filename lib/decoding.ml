(**
 * Low level representation of the operations and the details. Opcodes are converted to a
 * type and the rest of the word is encoded as a bitvec, then cut up according to the
 * lengths.
*)

(* Logical operations. From the variant field and the opcode field. *)
type operation =
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

(* ~~~ LOAD AND STORE OPERATIONS ~~~ *)

let mask_opcode_load = 0x3
let mask_opcode_store = 0x23

let mask_funct3_lw_and_sb = 0
let mask_funct3_lh = 0x1
let mask_funct3_lhu = 0x5
let mask_funct3_lb = 0x3
let mask_funct3_lbu = 0x4
let mask_funct3_sw = 0x2
let mask_funct3_sh = 0x1

(* ~~~ MEMORY ORDERING INSTRUCTIONS ~~~ *)

let mask_opcode_fence = 0xf

(* ~~~ ENVIRONMENT CALLING INSTRUCTIONS ~~~ *)

let mask_opcode_system = 0x73

let mask x = (1 lsl x) - 1

let unpack_field w i l = (w lsr i) land (mask l)

let unpack_operation w =
  let op = unpack_field w 0 7 in
  let funct3 = unpack_field w 12 3 in
  let funct7 = unpack_field w 25 7 in
  Printf.eprintf "OP: %x, funct3: %x, funct7: %x\n" op funct3 funct7;
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
  | op when op = mask_opcode_load -> (* LOAD *)
    (match funct3 with
     | f when f = mask_funct3_lw_and_sb -> LW
     | f when f = mask_funct3_lh -> LH
     | f when f = mask_funct3_lhu -> LHU
     | f when f = mask_funct3_lb -> LB
     | f when f = mask_funct3_lbu -> LBU
     | _ -> invalid_arg "unknown LOAD variant"
    )
  | op when op = mask_opcode_store -> (* STORE *)
    (match funct3 with
     | f when f = mask_funct3_sw -> SW
     | f when f = mask_funct3_sh -> SH
     | f when f = mask_funct3_lw_and_sb -> SB
     | _ -> invalid_arg "unknown STORE variant"
    )
  | op when op = mask_opcode_lui -> LUI
  | op when op = mask_opcode_auipc -> AUIPC
  | op when op = mask_opcode_jal -> JAL
  | op when op = mask_opcode_jalr -> JALR
  | op when op = mask_opcode_fence -> FENCE
  | op when op = mask_opcode_system ->
    let imm = unpack_field w 20 12 in
    if funct3 = 0 && imm = 0 then ECALL
    else if funct3 = 0 && imm = 1 then EBREAK
    else invalid_arg "unknown system"
  | _ -> invalid_arg "unknown op"

let decode_jal_imm w =
  let open Int32 in
  let imm20 = shift_left (logand (shift_right w 31) 0x1l) 20 in
  let imm10_1 = shift_left (logand (shift_right w 21) 0x3ffl) 1 in
  let imm11 = shift_left (logand (shift_right w 20) 0x1l) 11 in
  let imm19_12= shift_left (logand (shift_right w 12) 0xffl) 12 in
  let imm = logor imm20 (logor imm19_12 (logor imm11 imm10_1)) in
  shift_right (shift_left imm 11) 11

let decode_s_type t w =
  let imm_11_5 = ((w lsr 25) land 0x7f) lsl 5 in
  let imm_4_0  = (w lsr 7) land 0x1f in
  let imm = imm_11_5 lor imm_4_0 in
  let imm = (imm lsl 20) asr 20 in
  { t with
    t_rs1 = unpack_field w 15 5
  ; t_rs2 = unpack_field w 20 5
  ; t_imm = Int32.of_int imm }

let from w =
  let op = unpack_operation w in
  let t = { empty with t_operation = op } in
  let u = unpack_field w in
  match op with
  (* I-type instructions *)
  | ADDI | SLTI | SLTIU | ANDI
  | ORI | XORI | JALR | LH | LHU | LB | LBU | LW ->
    { t with
      t_rd = u 7 5
    ; t_rs1 = u 15 5
    ; t_imm = Int32.of_int (u 20 12) }
  | SLLI | SRLI | SRAI ->
    { t with
      t_rd = u 7 5
    ; t_rs1 = u 15 5
    ; t_imm = Int32.of_int (u 20 5) }
  | ECALL | EBREAK ->
    { t with t_rd = 0; t_rs1 = 0; t_imm = 0l }
  | FENCE ->
    { t with
      t_rd = u 7 5;
      t_rs1 = u 15 5;
      t_imm = Int32.of_int (((u 28 4) lsl 8) lor ((u 24 4) lsl 4) lor (u 20 4))
    }
  (* R-type instructions *)
  | ADD | SUB | SLT | SLTU | AND | OR | XOR
  | SLL | SRL | SRA ->
    { t with
      t_rd = u 7 5
    ; t_rs1 = u 15 5
    ; t_rs2 = u 20 5 }
  (* U-type instructions *)
  | LUI | AUIPC ->
    { t with
      t_rd = u 7 5
    ; t_imm = Int32.of_int (u 12 20) }
  (* J-type instruction *)
  | JAL ->
    { t with
      t_rd = u 7 5
    ; t_imm = decode_jal_imm (Int32.of_int w) }
  (* S-type instructions *)
  | SW | SH | SB -> decode_s_type t w
