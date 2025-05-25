
module Word = struct
  type t = int * int * int * int

  let zero = 0, 0, 0, 0

  let compare_unsigned_int a b =
    let a64 = Int64.logand (Int64.of_int a) 0xffffffffL in
    let b64 = Int64.logand (Int64.of_int b) 0xffffffffL in
    Int64.compare a64 b64

  let compare (a3, a2, a1, a0) (b3, b2, b1, b0) =
    let c3 = compare_unsigned_int a3 b3 in
    if c3 <> 0 then c3
    else
      let c2 = compare_unsigned_int a2 b2 in
      if c2 <> 0 then c2
      else
        let c1 = compare_unsigned_int a1 b1 in
        if c1 <> 0 then c1
        else compare_unsigned_int a0 b0
end

module Storage = Fmlib_std.Btree.Map (Word)

type state = { state_storage: Word.t Storage.t }

let state_empty = { state_storage = Storage.empty }
