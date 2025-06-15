
open Riscv32im_stylus

let compare_lifted_name x y =
  let open Lifted in
  let get_constructor_tag = function
    | Addi _ -> 0
    | Slti _ -> 1
    | Sltiu _ -> 2
    | Andi _ -> 3
    | Ori _ -> 4
    | Xori _ -> 5
    | Jalr _ -> 6
    | Lh _ -> 7
    | Lhu _ -> 8
    | Lb _ -> 9
    | Lbu _ -> 10
    | Lw _ -> 11
    | Slli _ -> 12
    | Srli _ -> 13
    | Srai _ -> 14
    | Ecall _ -> 15
    | Ebreak _ -> 16
    | Fence _ -> 17
    | Add _ -> 18
    | Sub _ -> 19
    | Slt _ -> 20
    | Sltu _ -> 21
    | And _ -> 22
    | Or _ -> 23
    | Xor _ -> 24
    | Sll _ -> 25
    | Srl _ -> 26
    | Sra _ -> 27
    | Mul _ -> 28
    | Mulh _ -> 29
    | Mulhsu _ -> 30
    | Mulhu _ -> 31
    | Div _ -> 32
    | Divu _ -> 33
    | Rem _ -> 34
    | Remu _ -> 35
    | Lui _ -> 36
    | Auipc _ -> 37
    | Jal _ -> 38
    | Sw _ -> 39
    | Sh _ -> 40
    | Sb _ -> 41
    | Beq _ -> 42
    | Bne _ -> 43
    | Blt _ -> 44
    | Bltu _ -> 45
    | Bge _ -> 46
    | Bgeu _ -> 47
  in
  let tag1 = get_constructor_tag x in
  let tag2 = get_constructor_tag y in
  compare tag1 tag2
