
type t = int32 Array.t

let gen =
  QCheck2.Gen.(array_size (int_range 0 100) int32)

let empty: t = [||]

let pp fmt arr =
  Format.fprintf fmt "[|";
  Array.iteri (fun i x ->
      if i > 0 then Format.fprintf fmt "; ";
      Format.fprintf fmt "%ld" x
    ) arr;
  Format.fprintf fmt "|]"
