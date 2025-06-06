(**
 * Low level representation of the operations and the details. Opcodes are converted to a
 * type and the rest of the word is encoded as a bitvec, then cut up according to the
 * lengths.
*)

(*
 * Decoded word in our high semi-high representation. We do not include the opcode,
 * funct3 or funct7 here! These fields are turned into the operation.
 *)
type t =
  { t_operation: Operation.t
  (** Destination register. *)
  ; t_rd: int
  (** The first register in an instruction. *)
  ; t_rs1: int
  (** The second register in an instruction. *)
  ; t_rs2: int
  (** The immediate literal embedded in the instruction. Variably sized. *)
  ; t_imm: int32 }
[@@deriving eq, make]

let sign_extend_12 x =
  let open Int32 in
  let x = logand x 0xfffl in
  if logand x 0x800l <> 0l then
    logor x (lognot 0xfffl)
  else
    x

let sign_extend_13 x =
  let open Int32 in
  let x = logand x 0x1fffl in
  if logand x 0x1000l <> 0l then
    logor x (lognot 0x1fffl)
  else x

let sign_extend_16 x =
  let open Int32 in
  let x = logand x 0xffffl in
  if logand x 0x8000l <> 0l then
    logor x (lognot 0xffffl)
  else x

let sign_extend_20 x =
  let open Int32 in
  let x = logand x 0xfffffl in
  if logand x 0x80000l <> 0l then
    logor x (lognot 0xfffffl)
  else
    x

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

(* ~~~ ENVIRONMENT CALLING INSTRUCTIONS ~~~ *)

let mask_opcode_system = 0x73

let mask x = (1 lsl x) - 1

let unpack_field w i l = (w lsr i) land (mask l)

let unpack_operation w =
  let open Operation in
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
  | op when op = mask_opcode_load -> (* LOAD *)
    (match funct3 with
     | f when f = mask_funct3_lw -> LW
     | f when f = mask_funct3_lh -> LH
     | f when f = mask_funct3_lhu -> LHU
     | f when f = mask_funct3_lb_and_sb -> LB
     | f when f = mask_funct3_lbu -> LBU
     | _ -> invalid_arg "unknown LOAD variant"
    )
  | op when op = mask_opcode_store -> (* STORE *)
    (match funct3 with
     | f when f = mask_funct3_sw -> SW
     | f when f = mask_funct3_sh -> SH
     | f when f = mask_funct3_lb_and_sb -> SB
     | _ -> invalid_arg "unknown STORE variant"
    )
  | op when op = mask_opcode_lui -> LUI
  | op when op = mask_opcode_auipc -> AUIPC
  | op when op = mask_opcode_jal -> JAL
  | op when op = mask_opcode_jalr -> JALR
  | op when op = mask_opcode_fence -> FENCE
  | op when op = mask_opcode_branch -> (* BRANCH *)
    (match funct3 with
    | f when f = mask_funct3_beq -> BEQ
    | f when f = mask_funct3_bne -> BNE
    | f when f = mask_funct3_blt -> BLT
    | f when f = mask_funct3_bltu -> BLTU
    | f when f = mask_funct3_bge -> BGE
    | f when f = mask_funct3_bgeu -> BGEU
    | _ -> invalid_arg "unknown BRANCH variant"
  )
  | op when op = mask_opcode_system ->
    let imm = unpack_field w 20 12 in
    if funct3 = 0 && imm = 0 then ECALL
    else if funct3 = 0 && imm = 1 then EBREAK
    else invalid_arg "unknown system"
  | _ -> invalid_arg "unknown op"

let decode_jal_imm w =
  let open Int32 in
  let imm_20 = shift_left (logand (shift_right w 31) 0x1l) 20 in
  let imm_10_1 = shift_left (logand (shift_right w 21) 0x3ffl) 1 in
  let imm_11 = shift_left (logand (shift_right w 20) 0x1l) 11 in
  let imm_19_12= shift_left (logand (shift_right w 12) 0xffl) 12 in
  let imm = logor imm_20 (logor imm_19_12 (logor imm_11 imm_10_1)) in
  shift_right (shift_left imm 11) 11

let decode_s_type t w =
  let imm_11_5 = ((w lsr 25) land 0x7f) lsl 5 in
  let imm_4_0  = (w lsr 7) land 0x1f in
  let imm = imm_11_5 lor imm_4_0 in
  let imm = sign_extend_12 (Int32.of_int imm) in
  { t with
    t_rs1 = unpack_field w 15 5
  ; t_rs2 = unpack_field w 20 5
  ; t_imm = imm }

let decode_b_type t w =
  let imm_12 = ((w lsr 31) land 0x1) lsl 12 in
  let imm_11 = ((w lsr 7) land 0x1) lsl 11 in
  let imm_10_5 = ((w lsr 25) land 0x3f) lsl 5 in
  let imm_4_1 = ((w lsr 8) land 0xf) lsl 1 in
  let imm = imm_12 lor imm_11 lor imm_10_5 lor imm_4_1 in
  let imm = sign_extend_13 (Int32.of_int imm) in
  { t with
    t_rs1 = unpack_field w 15 5
  ; t_rs2 = unpack_field w 20 5
  ; t_imm = imm }

let from w =
  let op = unpack_operation w in
  let t = { empty with t_operation = op } in
  let u = unpack_field w in
  match op with
  (* I-type instructions *)
  | ADDI | SLTI | SLTIU | ANDI
  | ORI | XORI | JALR | LH | LHU | LB | LBU | LW ->
    let imm = sign_extend_12 (Int32.of_int (u 20 12)) in
    { t with
      t_rd = u 7 5
    ; t_rs1 = u 15 5
    ; t_imm = imm }
  | SLLI | SRLI | SRAI ->
    let imm = sign_extend_12 (Int32.of_int (u 20 12)) in
    { t with
      t_rd = u 7 5
    ; t_rs1 = u 15 5
    ; t_imm = imm }
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
    let imm = sign_extend_20 (Int32.of_int (u 12 20)) in
    { t with
      t_rd = u 7 5
    ; t_imm = imm }
  (* J-type instruction *)
  | JAL ->
    { t with
      t_rd = u 7 5
    ; t_imm = decode_jal_imm (Int32.of_int w) }
  (* B-type instruction *)
  | BEQ | BNE | BLT | BLTU | BGE | BGEU -> decode_b_type t w
  (* S-type instructions *)
  | SW | SH | SB -> decode_s_type t w
