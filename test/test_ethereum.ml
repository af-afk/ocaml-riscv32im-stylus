open Riscv32im_stylus
open OUnit2

let gen_evm_word =
  let open QCheck2.Gen in
  let* w = Ethereum_word.gen in
  let* m = Memory.gen_evm_word in
  return (w, m)

let gen_rand_pair_mem =
  let open QCheck2.Gen in
  let* x = Ethereum_word.gen in
  let* y = Ethereum_word.gen in
  let* r = Memory.gen_evm_word_pair in
  return (x, y, r)

let pp_arr fmt arr =
  Format.fprintf fmt "0x";
  Array.iter (Format.fprintf fmt "%02x") arr

let to_and_from_word =
  QCheck_ounit.to_ounit2_test
  @@ QCheck2.Test.make ~name:"Eth word to and from (no array)"
       ~print:(fun (w, w') ->
         Format.asprintf "%a != %a" Ethereum_word.pp w Ethereum_word.pp w')
       (let open QCheck2.Gen in
        let* w = Ethereum_word.gen in
        let w' = Ethereum_word.(of_array (to_array w)) in
        return (w, w'))
       (fun (w, w') -> Ethereum_word.(equal w w'))

let store_from_and_to_words =
  QCheck_ounit.to_ounit2_test
  @@ QCheck2.Test.make ~name:"Eth word to, store, and from array"
       ~print:(fun (w, w') ->
         Format.asprintf "%a != %a" Ethereum_word.pp w Ethereum_word.pp w')
       (let open QCheck2.Gen in
        let* w, m = gen_evm_word in
        Memory.store_array m 0 (Ethereum_word.to_array w);
        return (w, Ethereum_word.of_array (Memory.load_into_array m 0 32l)))
       (fun (w, w') -> Ethereum_word.(equal w w'))

let store_load =
  let open QCheck2.Gen in
  QCheck_ounit.to_ounit2_test
  @@ QCheck2.Test.make ~name:"Store and load"
       ~print:(fun (k, v, k_arr, v_arr, test) ->
         let pp_word = Ethereum_word.pp in
         Format.asprintf
           "%a != %a. Key word before: %a, value word: %a. Key array: %a, \
            value array: %a"
           pp_word v pp_word test pp_word k pp_word v pp_arr k_arr pp_arr v_arr)
       ((* Store the value in the scratchpad: *)
        let* k, v, b = gen_rand_pair_mem in
        let c = Cpu.make ~b ~r:Registers.{ empty with t_r_a1 = 32l } () in
        let Cpu.{ b; _ } = c in
        let k_arr = Ethereum_word.to_array k in
        let v_arr = Ethereum_word.to_array v in
        Memory.store_array b 0 k_arr;
        Memory.store_array b 32 v_arr;
        (* Since A0 contains the location of the key, and A1 the value, we set A1
         * to 0 to set the start of the scratch we have. *)
        let Cpu.{ r; _ } = c in
        let c = { c with r = Registers.{ r with t_r_a1 = 0l } } in
        let Cpu.{ b; _ } = Ecalls.(ethereum_load c) in
        let test = Ethereum_word.of_array (Memory.load_into_array b 0 32l) in
        return (k, v, k_arr, v_arr, test))
       (fun (_, v, _, _, test) -> Ethereum_word.(equal v test))

let test =
  "Test Ethereum-specific operations"
  >::: [ store_load; to_and_from_word; store_from_and_to_words ]
