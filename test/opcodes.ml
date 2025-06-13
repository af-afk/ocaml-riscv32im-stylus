open Riscv32im_stylus.Lifted

let tag_i x i = match x with
  | `Addi -> Addi i
  | `Slti -> Slti i
  | `Sltiu -> Sltiu i
  | `Andi -> Andi i
  | `Ori -> Ori i
  | `Xori -> Xori i
  | `Jalr -> Jalr i
  | `Lh -> Lh i
  | `Lhu -> Lhu i
  | `Lb -> Lb i
  | `Lbu -> Lbu i
  | `Lw -> Lw i
  | `Fence -> Fence i

let tag_i_shift x i = match x with
  | `Slli -> Slli i
  | `Srli -> Srli i
  | `Srai -> Srai i

let tag_i_sys x i = match x with
  | `Ecall -> Ecall i
  | `Ebreak -> Ebreak i

let tag_r x i = match x with
  | `Add -> Add i
  | `Sub -> Sub i
  | `Slt -> Slt i
  | `Sltu -> Sltu i
  | `And -> And i
  | `Or -> Or i
  | `Xor -> Xor i
  | `Sll -> Sll i
  | `Srl -> Srl i
  | `Sra -> Sra i
  | `Mul -> Mul i
  | `Mulh -> Mulh i
  | `Mulhsu -> Mulhsu i
  | `Mulhu -> Mulhu i
  | `Div -> Div i
  | `Divu -> Divu i
  | `Rem -> Rem i
  | `Remu -> Remu i

let tag_u x i = match x with
  | `Lui -> Lui i
  | `Auipc -> Auipc i

let tag_j x i = match x with
  | `Jal -> Jal i

let tag_s x i = match x with
  | `Sw -> Sw i
  | `Sh -> Sh i
  | `Sb -> Sb i

let tag_b x i = match x with
  | `Beq -> Beq i
  | `Bne -> Bne i
  | `Blt -> Blt i
  | `Bltu -> Bltu i
  | `Bge -> Bge i
  | `Bgeu -> Bgeu i
