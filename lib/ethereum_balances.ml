
let compare_int32 = Int32.compare

type t = int32 [@@deriving qcheck2, compare, eq, show]

let empty = 0l
