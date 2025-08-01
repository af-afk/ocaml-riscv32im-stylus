let compare_int = Int.compare

type t =
  int * int * int * int * int * int * int * int *
  int * int * int * int * int * int * int * int *
  int * int * int * int * int * int * int * int *
  int * int * int * int * int * int * int * int
[@@deriving qcheck2, compare, eq]

let pp fmt (b31, b30, b29, b28, b27, b26, b25, b24,
            b23, b22, b21, b20, b19, b18, b17, b16,
            b15, b14, b13, b12, b11, b10, b9, b8,
            b7, b6, b5, b4, b3, b2, b1, b0) =
  let b = [b31; b30; b29; b28; b27; b26; b25; b24;
               b23; b22; b21; b20; b19; b18; b17; b16;
               b15; b14; b13; b12; b11; b10; b9; b8;
               b7; b6; b5; b4; b3; b2; b1; b0] in
  Format.fprintf fmt "0x%s"
    (String.concat "" (List.map (fun b -> Printf.sprintf "%02x" (b land 0xff)) b))

let zero = (0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0)

let max =
  let m = 255 in  (* max byte value *)
  (m, m, m, m, m, m, m, m,
   m, m, m, m, m, m, m, m,
   m, m, m, m, m, m, m, m,
   m, m, m, m, m, m, m, m)

let of_int x =
  let b0 = x land 0xff in
  let b1 = (x lsr 8) land 0xff in
  let b2 = (x lsr 16) land 0xff in
  let b3 = (x lsr 24) land 0xff in
  let b4 = (x lsr 32) land 0xff in
  let b5 = (x lsr 40) land 0xff in
  let b6 = (x lsr 48) land 0xff in
  let b7 = (x lsr 56) land 0xff in
  (0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0,
   b0, b1, b2, b3, b4, b5, b6, b7)

let to_array (b31, b30, b29, b28, b27, b26, b25, b24,
              b23, b22, b21, b20, b19, b18, b17, b16,
              b15, b14, b13, b12, b11, b10, b9, b8,
              b7, b6, b5, b4, b3, b2, b1, b0) =
  [|b31; b30; b29; b28; b27; b26; b25; b24;
    b23; b22; b21; b20; b19; b18; b17; b16;
    b15; b14; b13; b12; b11; b10; b9; b8;
    b7; b6; b5; b4; b3; b2; b1; b0|]

let of_array a =
  (a.(0), a.(1), a.(2), a.(3), a.(4), a.(5), a.(6), a.(7),
   a.(8), a.(9), a.(10), a.(11), a.(12), a.(13), a.(14), a.(15),
   a.(16), a.(17), a.(18), a.(19), a.(20), a.(21), a.(22), a.(23),
   a.(24), a.(25), a.(26), a.(27), a.(28), a.(29), a.(30), a.(31))
