
[@@@ocaml.warning "+A"]

module B = Fmlib_std.Btree.Map (Int)

module Registers = struct
  (**
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
    ; t_r_a7: int (* Also the ECALL number *) }
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
