
let mask x = (1 lsl x) - 1

let add_field value pos len acc = acc lor ((value land (mask len)) lsl pos)

let encode_i_type op Lifted.{ i_typ_imm ; i_typ_src ; i_typ_dst } =
  let imm_12 = Int32.to_int i_typ_imm in
  let funct3 = Lifted.get_funct3_mask op in
  let opcode = Lifted.get_opcode_mask op in
  add_field imm_12 20 12 0
  |> add_field (Registers.to_int i_typ_src) 15 5
  |> add_field funct3 12 3
  |> add_field (Registers.to_int i_typ_dst) 7 5
  |> add_field opcode 0 7
  |> Int32.of_int

let encode_i_type_shift op
    Lifted.{ i_typ_sft_imm ; i_typ_sft_src ; i_typ_sft_dst }
  =
  let shift_amt = Int32.to_int i_typ_sft_imm land 0x1f in
  let funct7 = match Lifted.get_funct7_mask op with Some f7 -> f7 | None -> 0 in
  let funct3 = Lifted.get_funct3_mask op in
  let opcode = Lifted.get_opcode_mask op in
  add_field funct7 25 7 0
  |> add_field shift_amt 20 5
  |> add_field (Registers.to_int i_typ_sft_src) 15 5
  |> add_field funct3 12 3
  |> add_field (Registers.to_int i_typ_sft_dst) 7 5
  |> add_field opcode 0 7
  |> Int32.of_int

let encode_i_type_system op (_: Lifted.i_typ_sys)  =
  let funct12 =
    match Lifted.get_funct12_mask op with
    | Some m -> m
    | None -> invalid_arg "Bad funct12 mask" in
  add_field funct12 20 12 0
  |> add_field (Registers.to_int `Zero) 15 5
  |> add_field 0 12 3
  |> add_field (Registers.to_int `Zero) 7 5
  |> add_field Operation.mask_opcode_system 0 7
  |> Int32.of_int

let encode_i_type_csr op
    Lifted.{ i_typ_csr_dst; i_typ_csr_addr; i_typ_csr_src }
  =
  let csr_addr = Int32.to_int i_typ_csr_addr land 0xfff in
  let funct3 = Lifted.get_funct3_mask op in
  let opcode = Lifted.get_opcode_mask op in
  add_field csr_addr 20 12 0
  |> add_field (Registers.to_int i_typ_csr_src) 15 5
  |> add_field funct3 12 3
  |> add_field (Registers.to_int i_typ_csr_dst) 7 5
  |> add_field opcode 0 7
  |> Int32.of_int

let encode_i_type_csr_imm op
    Lifted.{ i_typ_csr_imm_dst; i_typ_csr_imm_addr; i_typ_csr_imm_imm }
  =
  let csr_addr = Int32.to_int i_typ_csr_imm_addr land 0xfff in
  let imm5 = Int32.to_int i_typ_csr_imm_imm land 0x1f in
  let funct3 = Lifted.get_funct3_mask op in
  let opcode = Lifted.get_opcode_mask op in
  add_field csr_addr 20 12 0
  |> add_field imm5 15 5
  |> add_field funct3 12 3
  |> add_field (Registers.to_int i_typ_csr_imm_dst) 7 5
  |> add_field opcode 0 7
  |> Int32.of_int

let encode_j_type op Lifted.{ j_typ_dst ; j_typ_imm } =
  let imm_21 = Int32.to_int (Int32.logand j_typ_imm 0x1fffffl) in
  let imm_20 = (imm_21 lsr 20) land 0x1 in
  let imm_19_12 = (imm_21 lsr 12) land 0xff in
  let imm_11 = (imm_21 lsr 11) land 0x1 in
  let imm_10_1 = (imm_21 lsr 1) land 0x3ff in
  add_field imm_20 31 1 0
  |> add_field imm_10_1 21 10
  |> add_field imm_11 20 1
  |> add_field imm_19_12 12 8
  |> add_field (Registers.to_int j_typ_dst) 7 5
  |> add_field (Lifted.get_opcode_mask op) 0 7
  |> Int32.of_int

let encode_r_type op Lifted.{ r_typ_dst ; r_typ_src1 ; r_typ_src2 } =
  let funct7 =
    match Lifted.get_funct7_mask op with
    | Some f7 -> f7
    | None -> 0 in
  add_field funct7 25 7 0
  |> add_field (Registers.to_int r_typ_src2) 20 5
  |> add_field (Registers.to_int r_typ_src1) 15 5
  |> add_field (Lifted.get_funct3_mask op) 12 3
  |> add_field (Registers.to_int r_typ_dst) 7 5
  |> add_field (Lifted.get_opcode_mask op) 0 7
  |> Int32.of_int

let encode_u_type op Lifted.{ u_typ_dst ; u_typ_imm } =
  let imm_20 = Int32.to_int (Int32.shift_right_logical u_typ_imm 12) in
  add_field imm_20 12 20 0
  |> add_field (Registers.to_int u_typ_dst) 7 5
  |> add_field (Lifted.get_opcode_mask op) 0 7
  |> Int32.of_int

let encode_s_type op Lifted.{ s_typ_src1 ; s_typ_src2 ; s_typ_imm } =
  let imm_12 = Int32.to_int (Int32.logand s_typ_imm 0xfffl) in
  let imm_11_5 = (imm_12 lsr 5) land 0x7f in
  let imm_4_0 = imm_12 land 0x1f in
  0
  |> add_field imm_11_5 25 7
  |> add_field (Registers.to_int s_typ_src2) 20 5
  |> add_field (Registers.to_int s_typ_src1) 15 5
  |> add_field (Lifted.get_funct3_mask op) 12 3
  |> add_field imm_4_0 7 5
  |> add_field (Lifted.get_opcode_mask op) 0 7
  |> Int32.of_int

let encode_b_type op Lifted.{ b_typ_src1 ; b_typ_src2 ; b_typ_imm } =
  let imm_13 = Int32.to_int (Int32.logand b_typ_imm 0x1fffl) in
  let imm_12 = (imm_13 lsr 12) land 0x1 in
  let imm_11 = (imm_13 lsr 11) land 0x1 in
  let imm_10_5 = (imm_13 lsr 5) land 0x3f in
  let imm_4_1 = (imm_13 lsr 1) land 0xf in
  0
  |> add_field imm_12 31 1
  |> add_field imm_10_5 25 6
  |> add_field (Registers.to_int b_typ_src2) 20 5
  |> add_field (Registers.to_int b_typ_src1) 15 5
  |> add_field (Lifted.get_funct3_mask op) 12 3
  |> add_field imm_4_1 8 4
  |> add_field imm_11 7 1
  |> add_field (Lifted.get_opcode_mask op) 0 7
  |> Int32.of_int

let encode =
  let open Lifted in
  function
  (* I-type *)
  | Addi i_typ -> encode_i_type (Addi i_typ) i_typ
  | Slti i_typ -> encode_i_type (Slti i_typ) i_typ
  | Sltiu i_typ -> encode_i_type (Sltiu i_typ) i_typ
  | Andi i_typ -> encode_i_type (Andi i_typ) i_typ
  | Ori i_typ -> encode_i_type (Ori i_typ) i_typ
  | Xori i_typ -> encode_i_type (Xori i_typ) i_typ
  | Jalr i_typ -> encode_i_type (Jalr i_typ) i_typ
  | Lh i_typ -> encode_i_type (Lh i_typ) i_typ
  | Lhu i_typ -> encode_i_type (Lhu i_typ) i_typ
  | Lb i_typ -> encode_i_type (Lb i_typ) i_typ
  | Lbu i_typ -> encode_i_type (Lbu i_typ) i_typ
  | Lw i_typ -> encode_i_type (Lw i_typ) i_typ
  | Slli i_typ_sft -> encode_i_type_shift (Slli i_typ_sft) i_typ_sft
  | Srli i_typ_sft -> encode_i_type_shift (Srli i_typ_sft) i_typ_sft
  | Srai i_typ_sft -> encode_i_type_shift (Srai i_typ_sft) i_typ_sft
  | Ecall i_typ_sys -> encode_i_type_system (Ecall i_typ_sys) i_typ_sys
  | Ebreak i_typ_sys -> encode_i_type_system (Ebreak i_typ_sys) i_typ_sys
  | Fence i_typ -> encode_i_type (Fence i_typ) i_typ
  | Csrrw i_csr_typ -> encode_i_type_csr (Csrrw i_csr_typ) i_csr_typ
  | Csrrc i_csr_typ -> encode_i_type_csr (Csrrc i_csr_typ) i_csr_typ
  | Csrrs i_csr_typ -> encode_i_type_csr (Csrrs i_csr_typ) i_csr_typ
  | Csrrwi i_csr_imm_typ ->
    encode_i_type_csr_imm (Csrrwi i_csr_imm_typ) i_csr_imm_typ
  | Csrrsi i_csr_imm_typ ->
    encode_i_type_csr_imm (Csrrsi i_csr_imm_typ) i_csr_imm_typ
  | Csrrci i_csr_imm_typ ->
    encode_i_type_csr_imm (Csrrci i_csr_imm_typ) i_csr_imm_typ
  (* R-type *)
  | Add r_typ -> encode_r_type (Add r_typ) r_typ
  | Sub r_typ -> encode_r_type (Sub r_typ) r_typ
  | Slt r_typ -> encode_r_type (Slt r_typ) r_typ
  | Sltu r_typ -> encode_r_type (Sltu r_typ) r_typ
  | And r_typ -> encode_r_type (And r_typ) r_typ
  | Or r_typ -> encode_r_type (Or r_typ) r_typ
  | Xor r_typ -> encode_r_type (Xor r_typ) r_typ
  | Sll r_typ -> encode_r_type (Sll r_typ) r_typ
  | Srl r_typ -> encode_r_type (Srl r_typ) r_typ
  | Sra r_typ -> encode_r_type (Sra r_typ) r_typ
  | Mul r_typ -> encode_r_type (Mul r_typ) r_typ
  | Mulh r_typ -> encode_r_type (Mulh r_typ) r_typ
  | Mulhsu r_typ -> encode_r_type (Mulhsu r_typ) r_typ
  | Mulhu r_typ -> encode_r_type (Mulhu r_typ) r_typ
  | Div r_typ -> encode_r_type (Div r_typ) r_typ
  | Divu r_typ -> encode_r_type (Divu r_typ) r_typ
  | Rem r_typ -> encode_r_type (Rem r_typ) r_typ
  | Remu r_typ -> encode_r_type (Remu r_typ) r_typ
  (* U-type *)
  | Lui u_typ -> encode_u_type (Lui u_typ) u_typ
  | Auipc u_typ -> encode_u_type (Auipc u_typ) u_typ
  (* J-type *)
  | Jal j_typ -> encode_j_type (Jal j_typ) j_typ
  (* S-type *)
  | Sw s_typ -> encode_s_type (Sw s_typ) s_typ
  | Sh s_typ -> encode_s_type (Sh s_typ) s_typ
  | Sb s_typ -> encode_s_type (Sb s_typ) s_typ
  (* B-type *)
  | Beq b_typ -> encode_b_type (Beq b_typ) b_typ
  | Bne b_typ -> encode_b_type (Bne b_typ) b_typ
  | Blt b_typ -> encode_b_type (Blt b_typ) b_typ
  | Bltu b_typ -> encode_b_type (Bltu b_typ) b_typ
  | Bge b_typ -> encode_b_type (Bge b_typ) b_typ
  | Bgeu b_typ -> encode_b_type (Bgeu b_typ) b_typ

