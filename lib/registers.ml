
(**
 * Registers that the machine supports. Named after the convention I observed Spike
 * using.
*)
type t =
  { t_r_zero: int32 (* Always zero! *)
  ; t_r_ra: int32 (* For function calls *)
  ; t_r_sp: int32 (* Stack point32er *)
  ; t_r_gp: int32 (* Global variables point32er *)
  ; t_r_tp: int32 (* Thread point32er *)
  (* Temporary registers *)
  ; t_r_t0: int32 ; t_r_t1: int32 ; t_r_t2: int32
  ; t_r_t3: int32 ; t_r_t4: int32
  ; t_r_t5: int32 ; t_r_t6: int32
  (* Saved registers *)
  ; t_r_s0: int32 ; t_r_s1: int32
  ; t_r_s2: int32 ; t_r_s3: int32 ; t_r_s4: int32
  ; t_r_s5: int32 ; t_r_s6: int32 ; t_r_s7: int32
  ; t_r_s8: int32 ; t_r_s9: int32 ; t_r_s10: int32
  ; t_r_s11: int32
  (* Argument/return value registers *)
  ; t_r_a0: int32 ; t_r_a1: int32
  ; t_r_a2: int32 ; t_r_a3: int32 ; t_r_a4: int32
  ; t_r_a5: int32 ; t_r_a6: int32
  ; t_r_a7: int32 (* Also the ECALL number *) }
[@@deriving show, eq]

let empty =
  { t_r_zero = 0l; t_r_ra = 0l; t_r_sp = 0l; t_r_gp = 0l; t_r_tp = 0l
  ; t_r_t0 = 0l; t_r_t1 = 0l; t_r_t2 = 0l; t_r_s0 = 0l; t_r_s1 = 0l; t_r_a0 = 0l
  ; t_r_a1 = 0l; t_r_a2 = 0l; t_r_a3 = 0l; t_r_a4 = 0l; t_r_a5 = 0l; t_r_a6 = 0l
  ; t_r_a7 = 0l; t_r_s2 = 0l; t_r_s3 = 0l; t_r_s4 = 0l; t_r_s5 = 0l; t_r_s6 = 0l
  ; t_r_s7 = 0l; t_r_s8 = 0l; t_r_s9 = 0l; t_r_s10 = 0l; t_r_s11 = 0l; t_r_t3 = 0l
  ; t_r_t4 = 0l; t_r_t5 = 0l; t_r_t6 = 0l }

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

let of_int = function
  |  0 -> `Zero |  1 -> `Ra   |  2 -> `Sp
  |  3 -> `Gp   |  4 -> `Tp   |  5 -> `T0
  |  6 -> `T1   |  7 -> `T2   |  8 -> `S0
  |  9 -> `S1   | 10 -> `A0   | 11 -> `A1
  | 12 -> `A2   | 13 -> `A3   | 14 -> `A4
  | 15 -> `A5   | 16 -> `A6   | 17 -> `A7
  | 18 -> `S2   | 19 -> `S3   | 20 -> `S4
  | 21 -> `S5   | 22 -> `S6   | 23 -> `S7
  | 24 -> `S8   | 25 -> `S9   | 26 -> `S10
  | 27 -> `S11  | 28 -> `T3   | 29 -> `T4
  | 30 -> `T5   | 31 -> `T6   | _ -> invalid_arg "unknown register"

let update t dst x = match dst with
  | `Zero -> invalid_arg "zero was set"
  | `Ra -> { t with t_r_ra = x } | `Sp -> { t with t_r_sp = x } | `Gp -> { t with t_r_gp = x }
  | `Tp -> { t with t_r_tp = x } | `T0 -> { t with t_r_t0 = x } | `T1 -> { t with t_r_t1 = x }
  | `T2 -> { t with t_r_t2 = x } | `T3 -> { t with t_r_t3 = x } | `T4 -> { t with t_r_t4 = x }
  | `T5 -> { t with t_r_t5 = x } | `T6 -> { t with t_r_t6 = x } | `S0 -> { t with t_r_s0 = x }
  | `S1 -> { t with t_r_s1 = x } | `S2 -> { t with t_r_s2 = x } | `S3 -> { t with t_r_s3 = x }
  | `S4 -> { t with t_r_s4 = x } | `S5 -> { t with t_r_s5 = x } | `S6 -> { t with t_r_s6 = x }
  | `S7 -> { t with t_r_s7 = x } | `S8 -> { t with t_r_s8 = x } | `S9 -> { t with t_r_s9 = x }
  | `S10-> { t with t_r_s10= x } | `S11-> { t with t_r_s11= x } | `A0 -> { t with t_r_a0 = x }
  | `A1 -> { t with t_r_a1 = x } | `A2 -> { t with t_r_a2 = x } | `A3 -> { t with t_r_a3 = x }
  | `A4 -> { t with t_r_a4 = x } | `A5 -> { t with t_r_a5 = x } | `A6 -> { t with t_r_a6 = x }
  | `A7 -> { t with t_r_a7 = x }

let get t = function
  | `Zero -> 0l
  | `Ra -> t.t_r_ra  | `Sp -> t.t_r_sp  | `Gp -> t.t_r_gp
  | `Tp -> t.t_r_tp  | `T0 -> t.t_r_t0  | `T1 -> t.t_r_t1
  | `T2 -> t.t_r_t2  | `T3 -> t.t_r_t3  | `T4 -> t.t_r_t4
  | `T5 -> t.t_r_t5  | `T6 -> t.t_r_t6  | `S0 -> t.t_r_s0
  | `S1 -> t.t_r_s1  | `S2 -> t.t_r_s2  | `S3 -> t.t_r_s3
  | `S4 -> t.t_r_s4  | `S5 -> t.t_r_s5  | `S6 -> t.t_r_s6
  | `S7 -> t.t_r_s7  | `S8 -> t.t_r_s8  | `S9 -> t.t_r_s9
  | `S10-> t.t_r_s10 | `S11-> t.t_r_s11 | `A0 -> t.t_r_a0
  | `A1 -> t.t_r_a1  | `A2 -> t.t_r_a2  | `A3 -> t.t_r_a3
  | `A4 -> t.t_r_a4  | `A5 -> t.t_r_a5  | `A6 -> t.t_r_a6
  | `A7 -> t.t_r_a7