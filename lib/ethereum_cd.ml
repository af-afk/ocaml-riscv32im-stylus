
type t = int32 Array.t

let gen =
  QCheck2.Gen.(array_size (int_range 0 100) int32)

let empty = [||]

let pp fmt arr =
  Format.fprintf fmt "0x";
  Array.iter (Format.fprintf fmt "%02lx") arr
