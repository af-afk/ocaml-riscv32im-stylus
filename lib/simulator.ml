
(*
 * Bringing it all together, we need to simulate the Lifted
 * representation using the Registers, and the B interface for our block
 * storage.
 *)

type t =
  { r: Registers.t
  ; b: Memory.t
  ; e: Ethereum.t
  ; cd_b: Calldata.t
  ; rd_b: Calldata.t
  ; pc: int32 }
[@@deriving show, make, qcheck2]

let (+) = Int32.add

let bump_pc t = { t with pc = Int32.add 4l t.pc }

let sign_extend_8 x =
  let open Int32 in
  let x = logand x 0xffl in
  if logand x 0x80l <> 0l then
    logor x (lognot 0xffl)
  else x

let sign_extend_16 x =
  let open Int32 in
  let x = logand x 0xffffl in
  if logand x 0x8000l <> 0l then
    logor x (lognot 0xffffl)
  else x

[@@inline always]
let to_int x = match Int32.unsigned_to_int x with
  | Some v -> v
  | None -> failwith "Bad int conversion"

let step_addi t dst src imm =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst ((Registers.get r src) + imm) }

let step_slti t dst src imm =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (
          if Registers.get r src < imm then 1l else 0l
        ) }

let step_sltiu t dst src imm =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (
          if Int32.unsigned_compare (Registers.get r src) imm < 0 then 1l else 0l
        ) }

let step_andi t dst src imm =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (Int32.logand (Registers.get r src) imm) }

let step_ori t dst src imm =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (Int32.logor (Registers.get r src) imm) }

let step_xori t dst src imm =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (Int32.logxor (Registers.get r src) imm) }

let step_slli t dst src imm =
  let { r ; _ } = t in
  let bits = Int32.logand imm 0x1fl in
  bump_pc
    { t with r = Registers.update r dst (
          Int32.shift_left (Registers.get r src) (to_int bits)) }

let step_srli t dst src imm =
  let { r ; _ } = t in
  let bits = Int32.logand imm 0x1fl in
  bump_pc
    { t with r = Registers.update r dst (
          Int32.shift_right_logical (Registers.get r src) (to_int bits)) }

let step_srai t dst src imm =
  let { r ; _ } = t in
  let bits = Int32.logand imm 0x1fl in
  bump_pc
    { t with r = Registers.update r dst (
          Int32.shift_right (Registers.get r src) (to_int bits)) }

let step_jalr t dst src imm =
  let { r ; pc ; _ } = t in
  let target = Int32.add (Registers.get r src) imm in
  let target = Int32.logand target (Int32.lognot 1l) in
  { t with
    r = Registers.update r dst (Int32.add pc 4l)
  ; pc = target }

let step_lw t dst src imm =
  let { r ; b ; _ } = t in
  let addr = Int32.add (Registers.get r src) imm in
  let value = Memory.load_word_from_sim b addr in
  bump_pc { t with r = Registers.update r dst value }

let step_lh t dst src imm =
  let { r ; b ; _ } = t in
  let addr = Int32.add (Registers.get r src) imm in
  let value = sign_extend_16 (Memory.load_halfword_from_sim b addr) in
  bump_pc { t with r = Registers.update r dst value }

let step_lhu t dst src imm =
  let { r ; b ; _ } = t in
  let addr = Int32.add (Registers.get r src) imm in
  let value = Memory.load_halfword_unsigned_from_sim b addr in
  bump_pc { t with r = Registers.update r dst value }

let step_lb t dst src imm =
  let { r ; b ; _ } = t in
  let addr = Int32.add (Registers.get r src) imm in
  let value = sign_extend_8 (Memory.load_byte_sim b addr) in
  bump_pc { t with r = Registers.update r dst value }

let step_lbu t dst src imm =
  let { r ; b ; _ } = t in
  let addr = Int32.add (Registers.get r src) imm in
  let value = Memory.load_byte_unsigned_sim b addr in
  bump_pc { t with r = Registers.update r dst value }

let step_add t dst src1 src2 =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (
          Int32.add (Registers.get r src1) (Registers.get r src2)) }

let step_sub t dst src1 src2 =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (
          Int32.sub (Registers.get r src1) (Registers.get r src2)) }

let step_slt t dst src1 src2 =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (
          if (Registers.get r src1) < (Registers.get r src2) then 1l else 0l) }

let step_sltu t dst src1 src2 =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (
          if Int32.unsigned_compare (Registers.get r src1) (Registers.get r src2) < 0
          then 1l else 0l) }

let step_and t dst src1 src2 =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (
          Int32.logand (Registers.get r src1) (Registers.get r src2)) }

let step_or t dst src1 src2 =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (
          Int32.logor (Registers.get r src1) (Registers.get r src2)) }

let step_xor t dst src1 src2 =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (
          Int32.logxor (Registers.get r src1) (Registers.get r src2)) }

let step_sll t dst src1 src2 =
  let { r ; _ } = t in
  let shamt = to_int (Int32.logand (Registers.get r src2) 0x1fl) in
  bump_pc
    { t with r = Registers.update r dst (
          Int32.shift_left (Registers.get r src1) shamt) }

let step_srl t dst src1 src2 =
  let { r ; _ } = t in
  let shamt = to_int (Int32.logand (Registers.get r src2) 0x1fl) in
  bump_pc
    { t with r = Registers.update r dst (
          Int32.shift_right_logical (Registers.get r src1) shamt) }

let step_sra t dst src1 src2 =
  let { r ; _ } = t in
  let shamt = to_int (Int32.logand (Registers.get r src2) 0x1fl) in
  bump_pc
    { t with r = Registers.update r dst (
          Int32.shift_right (Registers.get r src1) shamt) }

let step_lui t dst imm =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (Int32.shift_left imm 12) }

let step_auipc t dst imm =
  let { r ; pc ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (Int32.add pc imm) }

let step_jal t dst imm =
  let { r ; pc ; _ } = t in
  { t with
    r = Registers.update r dst (Int32.add pc 4l);
    pc = Int32.add pc imm }  (* Imm already sign-extended from decode *)

let step_sw t src1 src2 imm =
  let { r ; b ; _ } = t in
  let base_addr = Registers.get r src1 in
  let addr = Int32.add base_addr imm in
  let value = Registers.get r src2 in
  Memory.store_word_sim b addr value;
  bump_pc t

let step_sh t src1 src2 imm =
  let { r ; b ; _ } = t in
  let addr = Int32.add (Registers.get r src1) imm in
  let value = Int32.logand (Registers.get r src2) 0xffffl in
  Memory.store_halfword_sim b addr value;
  bump_pc t

let step_sb t src1 src2 imm =
  let { r ; b ; _ } = t in
  let addr = Int32.add (Registers.get r src1) imm in
  let value = Int32.logand (Registers.get r src2) 0xffl in
  Memory.store_byte_sim b addr value;
  bump_pc t

let step_beq t src1 src2 imm =
  let { pc; r; _ } = t in
  if Registers.eq r src1 src2 then { t with pc = Int32.add pc imm }
  else bump_pc t

let step_bne t src1 src2 imm =
  let { pc; r; _ } = t in
  if not (Registers.eq r src1 src2) then { t with pc = Int32.add pc imm }
  else bump_pc t

let step_blt t src1 src2 imm =
  let { pc; r;_ } = t in
  if Registers.lt r src1 src2 then { t with pc = Int32.add pc imm }
  else bump_pc t

let step_bltu t src1 src2 imm =
  let { pc; r; _ } = t in
  if Registers.ltu r src1 src2 then { t with pc = Int32.add pc imm }
  else bump_pc t

let step_bge t src1 src2 imm =
  let { pc; r; _ } = t in
  if Registers.gte r src1 src2 then { t with pc = Int32.add pc imm }
  else bump_pc t

let step_bgeu t src1 src2 imm =
  let { pc; r; _ } = t in
  if Registers.gteu r src1 src2 then { t with pc = Int32.add pc imm }
  else bump_pc t

let step_fence t _ _ _ = bump_pc t

(* Outputs a string from the memory range given in a0, with length in a1. *)
let ecall_log_no = 0l

(* Store a word to the Ethereum storage tree. *)
let ecall_ethereum_store_no = 1l

(* Load a word from the Ethereum storage tree. *)
let ecall_ethereum_load_no = 2l

(* Get the size of the arguments that were passed to the contract. *)
let ecall_ethereum_args_no = 3l

(* Write the output from this program. *)
let ecall_ethereum_output_no = 4l

let ecall_log fmt t from length =
  let { b; _ } = t in
  Format.fprintf fmt "%s" (Memory.load_into_str_sim b from length);
  Format.pp_force_newline fmt ();
  t

let ecall_ethereum_store t ptr_key ptr_val =
  let { e; b; _ } = t in
  let key =
    Ethereum.Word.of_array (Memory.load_into_array b ptr_key 32l) in
  let v =
    Ethereum.Word.of_array (Memory.load_into_array b ptr_val 32l) in
  { t with e = Ethereum.store_word e key v }

let ecall_ethereum_load t ptr_key ptr_write =
  let { e; b; _ } = t in
  let key =
    Ethereum.Word.of_array (Memory.load_into_array b ptr_key 32l) in
  let v = Ethereum.load_word e key in
  Memory.store_array b ptr_write (Ethereum.Word.to_array v)

let step_ecall fmt t _ _ _ =
  let { r; _ } = t in
  match Registers.get r `A7 with
  | s when s = ecall_log_no ->
    bump_pc (
      ecall_log fmt t (Registers.get r `A0) (Registers.get r `A1))
  | r -> failwith (Printf.sprintf "Bad register: %ld" r)

let step_ebreak _ _ _ _ = failwith "BREAK"

let step_mul t dst src1 src2 =
  let { r ; _ } = t in
  bump_pc
    { t with r = Registers.update r dst (
          Int32.mul (Registers.get r src1) (Registers.get r src2)) }

let step_mulh t dst src1 src2 =
  let { r ; _ } = t in
  let a = Int64.of_int32 (Registers.get r src1) in
  let b = Int64.of_int32 (Registers.get r src2) in
  let result = Int64.mul a b in
  let high = Int64.to_int32 (Int64.shift_right result 32) in
  bump_pc { t with r = Registers.update r dst high }

let step_mulhsu t dst src1 src2 =
  let { r ; _ } = t in
  let a = Int64.of_int32 (Registers.get r src1) in
  let b = Int64.logand 0xffffffffL (Int64.of_int32 (Registers.get r src2)) in
  let result = Int64.mul a b in
  let high = Int64.to_int32 (Int64.shift_right result 32) in
  bump_pc { t with r = Registers.update r dst high }

let step_mulhu t dst src1 src2 =
  let { r ; _ } = t in
  let a = Int64.logand 0xffffffffL (Int64.of_int32 (Registers.get r src1)) in
  let b = Int64.logand 0xffffffffL (Int64.of_int32 (Registers.get r src2)) in
  let result = Int64.mul a b in
  let high = Int64.to_int32 (Int64.shift_right_logical result 32) in
  bump_pc { t with r = Registers.update r dst high }

let step_div t dst src1 src2 =
  let { r ; _ } = t in
  let dividend = Registers.get r src1 in
  let divisor = Registers.get r src2 in
  let result =
    if divisor = 0l then -1l  (* Division by zero *)
    else if dividend = Int32.min_int && divisor = -1l then dividend
    else Int32.div dividend divisor in
  bump_pc { t with r = Registers.update r dst result }

let step_divu t dst src1 src2 =
  let { r ; _ } = t in
  let dividend = Registers.get r src1 in
  let divisor = Registers.get r src2 in
  let result =
    if divisor = 0l then -1l  (* Division by zero *)
    else Int32.unsigned_div dividend divisor in
  bump_pc { t with r = Registers.update r dst result }

let step_rem t dst src1 src2 =
  let { r ; _ } = t in
  let dividend = Registers.get r src1 in
  let divisor = Registers.get r src2 in
  let result =
    if divisor = 0l then dividend
    else if dividend = Int32.min_int && divisor = -1l then 0l
    else Int32.rem dividend divisor in
  bump_pc { t with r = Registers.update r dst result }

let step_remu t dst src1 src2 =
  let { r ; _ } = t in
  let dividend = Registers.get r src1 in
  let divisor = Registers.get r src2 in
  let result =
    if divisor = 0l then dividend
    else Int32.unsigned_rem dividend divisor in
  bump_pc { t with r = Registers.update r dst result }

let step_lifted fmt t f =
  let open Lifted in
  let apply_i { i_typ_dst; i_typ_src; i_typ_imm } f =
    f t i_typ_dst i_typ_src i_typ_imm in
  let apply_i_sft { i_typ_sft_dst; i_typ_sft_src; i_typ_sft_imm } f =
    f t i_typ_sft_dst i_typ_sft_src i_typ_sft_imm in
  let apply_i_sys { i_typ_sys_dst; i_typ_sys_src; i_typ_sys_imm } f =
    f t i_typ_sys_dst i_typ_sys_src i_typ_sys_imm in
  let apply_r { r_typ_dst; r_typ_src1; r_typ_src2 } f =
    f t r_typ_dst r_typ_src1 r_typ_src2 in
  let apply_u { u_typ_dst; u_typ_imm } f =
    f t u_typ_dst u_typ_imm in
  let apply_s { s_typ_src1; s_typ_src2; s_typ_imm } f =
    f t s_typ_src1 s_typ_src2 s_typ_imm in
  let apply_j { j_typ_dst; j_typ_imm } f =
    f t j_typ_dst j_typ_imm in
  let apply_b { b_typ_src1; b_typ_src2; b_typ_imm } f =
    f t b_typ_src1 b_typ_src2 b_typ_imm in
  match f with
  (* I-type *)
  | Addi f -> apply_i f step_addi
  | Slti f -> apply_i f step_slti
  | Sltiu f -> apply_i f step_sltiu
  | Andi f -> apply_i f step_andi
  | Ori f -> apply_i f step_ori
  | Xori f -> apply_i f step_xori
  | Slli f -> apply_i_sft f step_slli
  | Srli f -> apply_i_sft f step_srli
  | Srai f -> apply_i_sft f step_srai
  | Jalr f -> apply_i f step_jalr
  | Lw f -> apply_i f step_lw
  | Lh f -> apply_i f step_lh
  | Lhu f -> apply_i f step_lhu
  | Lb f -> apply_i f step_lb
  | Lbu f -> apply_i f step_lbu
  | Fence f -> apply_i f step_fence
  | Ecall f -> apply_i_sys f (step_ecall fmt)
  | Ebreak f -> apply_i_sys f step_ebreak
  (* R-type *)
  | Add f -> apply_r f step_add
  | Sub f -> apply_r f step_sub
  | Slt f -> apply_r f step_slt
  | Sltu f -> apply_r f step_sltu
  | And f -> apply_r f step_and
  | Or f -> apply_r f step_or
  | Xor f -> apply_r f step_xor
  | Sll f -> apply_r f step_sll
  | Srl f -> apply_r f step_srl
  | Sra f -> apply_r f step_sra
  | Mul f -> apply_r f step_mul
  | Mulh f -> apply_r f step_mulh
  | Mulhsu f -> apply_r f step_mulhsu
  | Mulhu f -> apply_r f step_mulhu
  | Div f -> apply_r f step_div
  | Divu f -> apply_r f step_divu
  | Rem f -> apply_r f step_rem
  | Remu f -> apply_r f step_remu
  (* U-type *)
  | Lui f -> apply_u f step_lui
  | Auipc f -> apply_u f step_auipc
  (* J-type *)
  | Jal f -> apply_j f step_jal
  (* S-type *)
  | Sw f -> apply_s f step_sw
  | Sh f -> apply_s f step_sh
  | Sb f -> apply_s f step_sb
  (* B-type *)
  | Beq f -> apply_b f step_beq
  | Bne f -> apply_b f step_bne
  | Blt f -> apply_b f step_blt
  | Bltu f -> apply_b f step_bltu
  | Bge f -> apply_b f step_bge
  | Bgeu f -> apply_b f step_bgeu

let id x = x

let step ?(before_lift = id) ?(after_lift = id) fmt t =
  let { b ; pc ; _ } = t in
  let word = before_lift (Memory.load_word_from_sim b pc) in
  let word_int = to_int word in
  let instr =
    try after_lift (Lifted.from_word pc word_int) with err -> (
        pp fmt t;
        Format.pp_force_newline fmt ();
        Format.fprintf fmt "Loaded word: %ld, (%d)" word word_int;
        Format.pp_force_newline fmt ();
        raise err
      ) in
  try step_lifted fmt t instr with err -> (
      pp fmt t;
      Lifted.pp fmt instr;
      raise err
    )
