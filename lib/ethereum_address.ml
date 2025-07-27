let compare_int32 = Int32.compare

type t = int32 * int32 * int32 * int32 * int32 [@@deriving qcheck2, compare, eq]

let pp fmt (w4, w3, w2, w1, w0) =
  Format.fprintf fmt "0x%08lx%08lx%08lx%08lx%08lx" w4 w3 w2 w1 w0

let zero = 0l, 0l, 0l, 0l, 0l
