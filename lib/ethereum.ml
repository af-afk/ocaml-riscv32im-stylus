
module Word = struct
  type t = int32 * int32 * int32 * int32 [@@deriving show, qcheck2]

  let zero = 0l, 0l, 0l, 0l

  let compare (a3, a2, a1, a0) (b3, b2, b1, b0) =
    let c3 = Int32.compare a3 b3 in
    if c3 <> 0 then c3
    else
      let c2 = Int32.compare a2 b2 in
      if c2 <> 0 then c2
      else
        let c1 = Int32.compare a1 b1 in
        if c1 <> 0 then c1
        else Int32.compare a0 b0

  let to_array (w3, w2, w1, w0) =
    let arr = Array.make 16 0l in
    let open Int32 in
    arr.(0) <- logand (shift_right_logical w3 24) 0xffl;
    arr.(1) <- logand (shift_right_logical w3 16) 0xffl;
    arr.(2) <- logand (shift_right_logical w3 8) 0xffl;
    arr.(3) <- logand w3 0xffl;
    arr.(4) <- logand (shift_right_logical w2 24) 0xffl;
    arr.(5) <- logand (shift_right_logical w2 16) 0xffl;
    arr.(6) <- logand (shift_right_logical w2 8) 0xffl;
    arr.(7) <- logand w2 0xffl;
    arr.(8) <- logand (shift_right_logical w1 24) 0xffl;
    arr.(9) <- logand (shift_right_logical w1 16) 0xffl;
    arr.(10) <- logand (shift_right_logical w1 8) 0xffl;
    arr.(11) <- logand w1 0xffl;
    arr.(12) <- logand (shift_right_logical w0 24) 0xffl;
    arr.(13) <- logand (shift_right_logical w0 16) 0xffl;
    arr.(14) <- logand (shift_right_logical w0 8) 0xffl;
    arr.(15) <- logand w0 0xffl;
    arr

  let of_array a =
    let open Int32 in
    let w3 =
      logor (logor (logor
                      (shift_left (of_int a.(0)) 24)
                      (shift_left (of_int a.(1)) 16))
               (shift_left (of_int a.(2)) 8))
        (of_int a.(3)) in
    let w2 =
      logor (logor (logor
                      (shift_left (of_int a.(4)) 24)
                      (shift_left (of_int a.(5)) 16))
               (shift_left (of_int a.(6)) 8))
        (of_int a.(7)) in
    let w1 =
      logor (logor (logor
                      (shift_left (of_int a.(8)) 24)
                      (shift_left (of_int a.(9)) 16))
               (shift_left (of_int a.(10)) 8))
        (of_int a.(11)) in
    let w0 =
      logor (logor (logor
                      (shift_left (of_int a.(12)) 24)
                      (shift_left (of_int a.(13)) 16))
               (shift_left (of_int a.(14)) 8))
        (of_int a.(15)) in
    (w3, w2, w1, w0)
end

module Storage = struct
  module M = Map.Make (Word)
  include M

  let pp pp_val fmt map =
    Format.fprintf fmt "@[<v>{";
    M.iter (fun k v ->
        Format.fprintf fmt "@ @[<hov 2>%a =>@ %a;@]" Word.pp k pp_val v
      ) map;
    Format.fprintf fmt "@ }@]"

  let gen value_gen =
    let open QCheck2.Gen in
    list_size (int_range 0 100) (pair Word.gen value_gen)
    |> map (List.fold_left (fun m (k, v) -> add k v m) empty)
end

type t = Word.t Storage.t [@@deriving show, qcheck2]

let empty = Storage.empty

let load_word t k =
  Option.value ~default:Word.zero (Storage.find_opt k t)

let store_word t k v = Storage.add k v t
