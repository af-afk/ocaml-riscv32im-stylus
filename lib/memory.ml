
module Region = struct
  type arr = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  type t =
    { mem: arr
    ; base: int
    ; size: int
    ; readable: bool
    ; writeable: bool
    ; executable: bool }
end

type t = Region.t list

let load_word _ _ = failwith "TODO"
let load_halfword _ _ = failwith "TODO"
let load_halfword_unsigned _ _ = failwith "TODO"
let load_byte _ _ = failwith "TODO"
let load_byte_unsigned _ _ = failwith "TODO"
let store_word _ _ _ = failwith "TODO"
let store_halfword _ _ _ = failwith "TODO"
let store_byte _ _ _ = failwith "TODO"
