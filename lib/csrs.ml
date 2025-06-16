
module M = Map.Make (Int32)

type t = int32 M.t [@@deriving eq]

let empty = M.empty

let gen =
  let open QCheck2.Gen in
  let* pairs = list (pair int32 int32) in
  return (M.of_seq (List.to_seq pairs))

let pp fmt t =
  let pp_entry fmt (key, value) =
    Format.fprintf fmt "@[%ld -> %ld@]" key value in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
    pp_entry
    fmt
    (M.bindings t)

let get t k =
  match M.find_opt k t with
  | Some v -> v
  | None -> 0l

let set t k v =
  M.update k (fun _ -> Some v) t
