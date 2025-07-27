
module Storage = struct
  module M = Map.Make (Ethereum_word)
  include M

  let pp pp_val fmt map =
    Format.fprintf fmt "@[<v>{";
    M.iter (fun k v ->
        Format.fprintf fmt "@ @[<hov 2>%a =>@ %a;@]" Ethereum_word.pp k pp_val v
      ) map;
    Format.fprintf fmt "@ }@]"

  let gen value_gen =
    let open QCheck2.Gen in
    list_size (int_range 0 100) (pair Ethereum_word.gen value_gen)
    |> map (List.fold_left (fun m (k, v) -> add k v m) empty)
end

type t = Ethereum_word.t Storage.t [@@deriving show, qcheck2]

let empty = Storage.empty

let load_word t k =
  Option.value ~default:Ethereum_word.zero (Storage.find_opt k t)

let store_word t k v = Storage.add k v t
