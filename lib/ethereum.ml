
module Word = struct
  let compare_int32 = Int32.compare

  type t = int32 * int32 * int32 * int32 * int32 * int32 * int32 * int32
  [@@deriving qcheck2, compare,eq]

  let pp fmt (w7, w6, w5, w4, w3, w2, w1, w0) =
    Format.fprintf fmt "0x%08lx%08lx%08lx%08lx%08lx%08lx%08lx%08lx"
      w7 w6 w5 w4 w3 w2 w1 w0

  let zero = 0l, 0l, 0l, 0l, 0l, 0l, 0l, 0l

  let to_array (w7, w6, w5, w4, w3, w2, w1, w0) =
    let arr = Array.make 32 0l in
    let open Int32 in
    arr.(0) <- logand (shift_right_logical w7 24) 0xffl;
    arr.(1) <- logand (shift_right_logical w7 16) 0xffl;
    arr.(2) <- logand (shift_right_logical w7 8) 0xffl;
    arr.(3) <- logand w7 0xffl;
    arr.(4) <- logand (shift_right_logical w6 24) 0xffl;
    arr.(5) <- logand (shift_right_logical w6 16) 0xffl;
    arr.(6) <- logand (shift_right_logical w6 8) 0xffl;
    arr.(7) <- logand w6 0xffl;
    arr.(8) <- logand (shift_right_logical w5 24) 0xffl;
    arr.(9) <- logand (shift_right_logical w5 16) 0xffl;
    arr.(10) <- logand (shift_right_logical w5 8) 0xffl;
    arr.(11) <- logand w5 0xffl;
    arr.(12) <- logand (shift_right_logical w4 24) 0xffl;
    arr.(13) <- logand (shift_right_logical w4 16) 0xffl;
    arr.(14) <- logand (shift_right_logical w4 8) 0xffl;
    arr.(15) <- logand w4 0xffl;
    arr.(16) <- logand (shift_right_logical w3 24) 0xffl;
    arr.(17) <- logand (shift_right_logical w3 16) 0xffl;
    arr.(18) <- logand (shift_right_logical w3 8) 0xffl;
    arr.(19) <- logand w3 0xffl;
    arr.(20) <- logand (shift_right_logical w2 24) 0xffl;
    arr.(21) <- logand (shift_right_logical w2 16) 0xffl;
    arr.(22) <- logand (shift_right_logical w2 8) 0xffl;
    arr.(23) <- logand w2 0xffl;
    arr.(24) <- logand (shift_right_logical w1 24) 0xffl;
    arr.(25) <- logand (shift_right_logical w1 16) 0xffl;
    arr.(26) <- logand (shift_right_logical w1 8) 0xffl;
    arr.(27) <- logand w1 0xffl;
    arr.(28) <- logand (shift_right_logical w0 24) 0xffl;
    arr.(29) <- logand (shift_right_logical w0 16) 0xffl;
    arr.(30) <- logand (shift_right_logical w0 8) 0xffl;
    arr.(31) <- logand w0 0xffl;
    arr

  let of_array a =
    let open Int32 in
    let w7 =
      logor (logor (logor
                      (shift_left (of_int a.(0)) 24)
                      (shift_left (of_int a.(1)) 16))
               (shift_left (of_int a.(2)) 8))
        (of_int a.(3)) in
    let w6 =
      logor (logor (logor
                      (shift_left (of_int a.(4)) 24)
                      (shift_left (of_int a.(5)) 16))
               (shift_left (of_int a.(6)) 8))
        (of_int a.(7)) in
    let w5 =
      logor (logor (logor
                      (shift_left (of_int a.(8)) 24)
                      (shift_left (of_int a.(9)) 16))
               (shift_left (of_int a.(10)) 8))
        (of_int a.(11)) in
    let w4 =
      logor (logor (logor
                      (shift_left (of_int a.(12)) 24)
                      (shift_left (of_int a.(13)) 16))
               (shift_left (of_int a.(14)) 8))
        (of_int a.(15)) in
    let w3 =
      logor (logor (logor
                      (shift_left (of_int a.(16)) 24)
                      (shift_left (of_int a.(17)) 16))
               (shift_left (of_int a.(18)) 8))
        (of_int a.(19)) in
    let w2 =
      logor (logor (logor
                      (shift_left (of_int a.(20)) 24)
                      (shift_left (of_int a.(21)) 16))
               (shift_left (of_int a.(22)) 8))
        (of_int a.(23)) in
    let w1 =
      logor (logor (logor
                      (shift_left (of_int a.(24)) 24)
                      (shift_left (of_int a.(25)) 16))
               (shift_left (of_int a.(26)) 8))
        (of_int a.(27)) in
    let w0 =
      logor (logor (logor
                      (shift_left (of_int a.(28)) 24)
                      (shift_left (of_int a.(29)) 16))
               (shift_left (of_int a.(30)) 8))
        (of_int a.(31)) in
    (w7, w6, w5, w4, w3, w2, w1, w0)
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
