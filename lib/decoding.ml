(**
 * Low level representation of the operations and the details. Opcodes are converted to a
 * type and the rest of the word is encoded as a bitvec, then cut up according to the
 * lengths.
*)

open Sexplib0.Sexp_conv

(*
 * Decoded word in our high semi-high representation. We do not include the opcode,
 * funct3 or funct7 here! These fields are turned into the operation.
 *)
type t =
  (** Location of the operation. *)
  { loc: int32
  (** The word before it was decoded. *)
  ; word: int
  ; operation: Operation.t
  (** Destination register. *)
  ; rd: int
  (** The first register in an instruction. *)
  ; rs1: int
  (** The second register in an instruction. *)
  ; rs2: int
  (** The immediate literal embedded in the instruction. *)
  ; imm: int32 }
[@@deriving eq, make, sexp, show]

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
  { loc = 0l
  ; word = 0
  ; operation = ADDI
  ; rd = 0
  ; rs1 = 0
  ; rs2 = 0
  ; imm = 0l }

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
     | f when f = mask_funct3_add_and_sub && funct7 = mask_funct7_mul -> MUL
     | f when f = mask_funct3_add_and_sub -> ADD
     | f when f = mask_funct3_sll && funct7 = mask_funct7_mulh -> MULH
     | f when f = mask_funct3_sll -> SLL
     | f when f = mask_funct3_slti && funct7 = mask_funct7_mulhsu -> MULHSU
     | f when f = mask_funct3_slti -> SLT
     | f when f = mask_funct3_sltiu && funct7 = mask_funct7_mulhu -> MULHU
     | f when f = mask_funct3_sltiu -> SLTU
     | f when f = mask_funct3_xor && funct7 = mask_funct7_div -> DIV
     | f when f = mask_funct3_xor -> XOR
     | f when f = mask_funct3_srl_and_sra && funct7 = mask_funct7_sra -> SRA
     | f when f = mask_funct3_srl_and_sra && funct7 = mask_funct7_divu -> DIVU
     | f when f = mask_funct3_srl_and_sra -> SRL
     | f when f = mask_funct3_or && funct7 = mask_funct7_rem -> REM
     | f when f = mask_funct3_or -> OR
     | f when f = mask_funct3_and && funct7 = mask_funct7_remu -> REMU
     | f when f = mask_funct3_and -> AND
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
    (match funct3, unpack_field w 20 12 with
     | 0, funct12 when funct12 = mask_funct12_ecall ->
       ECALL
     | 0, funct12 when funct12 = mask_funct12_ebreak ->
       EBREAK
     | funct3, _ when funct3 = mask_funct3_csrrw ->
       CSRRW
     | funct3, _ when funct3 = mask_funct3_csrrs ->
       CSRRS
     | funct3, _ when funct3 = mask_funct3_csrrc ->
       CSRRC
     | funct3, _ when funct3 = mask_funct3_csrrwi ->
       CSRRWI
     | funct3, _ when funct3 = mask_funct3_csrrsi ->
       CSRRSI
     | funct3, _ when funct3 = mask_funct3_csrrci ->
       CSRRCI
     | _ -> invalid_arg "invalid SYSTEM variant"
    )
  | _ -> invalid_arg (Printf.sprintf "unknown op: %x" w)

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
    rs1 = unpack_field w 15 5
  ; rs2 = unpack_field w 20 5
  ; imm = imm }

let decode_b_type t w =
  let imm_12 = ((w lsr 31) land 0x1) lsl 12 in
  let imm_11 = ((w lsr 7) land 0x1) lsl 11 in
  let imm_10_5 = ((w lsr 25) land 0x3f) lsl 5 in
  let imm_4_1 = ((w lsr 8) land 0xf) lsl 1 in
  let imm = imm_12 lor imm_11 lor imm_10_5 lor imm_4_1 in
  let imm = sign_extend_13 (Int32.of_int imm) in
  { t with
    rs1 = unpack_field w 15 5
  ; rs2 = unpack_field w 20 5
  ; imm = imm }

let from loc w =
  let op = unpack_operation w in
  let t = { empty with operation = op ; word = w; loc } in
  let u = unpack_field w in
  match op with
  (* I-type instructions *)
  | ADDI | SLTI | SLTIU | ANDI
  | ORI | XORI | JALR | LH | LHU | LB | LBU | LW ->
    let imm = sign_extend_12 (Int32.of_int (u 20 12)) in
    { t with
      rd = u 7 5
    ; rs1 = u 15 5
    ; imm = imm }
  | SLLI | SRLI | SRAI ->
    { t with
      rd = u 7 5
    ; rs1 = u 15 5
    ; imm = Int32.of_int (u 20 5) }
  | ECALL | EBREAK -> { t with rd = 0; rs1 = 0; imm = 0l }
  | CSRRW | CSRRS | CSRRC ->
    let csr = Int32.of_int (u 20 12) in
    { t with rd = u 7 5; rs1 = u 15 5; imm = csr }
  | CSRRWI | CSRRSI | CSRRCI ->
    let csr = Int32.of_int (u 20 12) in
    { t with rd = u 7 5; rs1 = u 15 5; imm = csr }
  | FENCE ->
    let fence_bits = ((u 28 4) lsl 8) lor ((u 24 4) lsl 4) lor (u 20 4) in
    let imm = sign_extend_12 (Int32.of_int fence_bits) in
    { t with
      rd = u 7 5
    ; rs1 = u 15 5
    ; imm = imm }
  (* R-type instructions *)
  | ADD | SUB | SLT | SLTU | AND | OR | XOR
  | SLL | SRL | SRA | MUL | MULH | MULHSU
  | MULHU | DIV | DIVU | REM | REMU ->
    { t with
      rd = u 7 5
    ; rs1 = u 15 5
    ; rs2 = u 20 5 }
  (* U-type instructions *)
  | LUI | AUIPC ->
    let imm = Int32.(shift_left (of_int (u 12 20)) 12) in
    { t with
      rd = u 7 5
    ; imm = imm }
  (* J-type instruction *)
  | JAL ->
    { t with
      rd = u 7 5
    ; imm = decode_jal_imm (Int32.of_int w) }
  (* B-type instruction *)
  | BEQ | BNE | BLT | BLTU | BGE | BGEU -> decode_b_type t w
  (* S-type instructions *)
  | SW | SH | SB -> decode_s_type t w
