
open Riscv32im_stylus

open Storage

let random_byte_store_read =
  let open QCheck2 in
  QCheck_ounit.to_ounit2_test @@ Test.make
    ~name:"store byte/read byte"
    ~print:(fun (addr, w, m) ->
        let b = Int32.logand w 0xffl in
        let addr = Int32.of_int addr in
        Memory.store_byte_sim m addr b;
        let loaded = Memory.load_byte_sim m addr in
        Format.asprintf "Addr: %lx, word: %lx, mem: %a, LOADED: %lx, EXPECTED: %lx"
          addr
          w
          Memory.pp m
          loaded
          b
      )
    (gen_random_word Memory.gen)
    (fun (addr, w, m) ->
       let addr = Int32.of_int addr in
       let b = Int32.logand w 0xffl in
       Memory.store_byte_sim m addr b;
       b = Memory.load_byte_sim m addr
    )

let random_word_store_read =
  let open QCheck2 in
  QCheck_ounit.to_ounit2_test @@ Test.make
    ~name:"store word/read word"
    ~print:(fun (addr, w, m) ->
        let addr = Int32.of_int addr in
        try
          Memory.store_word_sim m addr w;
          let loaded = Memory.load_word_from_sim m addr in
          Format.asprintf "Addr: %lx, mem: %a, LOADED: %lx, EXPECTED: %lx"
            addr
            Memory.pp m
            loaded
            w
        with _ -> (
            Format.asprintf "Addr: %lx, mem: %a, ERROR READING!"
              addr
              Memory.pp m
          )
      )
    (gen_random_word Memory.gen)
    (fun (addr, w, m) ->
       let addr = Int32.of_int addr in
       Memory.store_word_sim m addr w;
       w = Memory.load_word_from_sim m addr
    )

let reference_word_store_read m =
  let open QCheck2 in
  let m = List.rev_map (fun t ->
      Memory.Region.{ t with readable = true ; writeable = true }) m in
  QCheck_ounit.to_ounit2_test @@ Test.make
    ~name:"test binary store word/read word"
    ~print:(fun (addr, w, m) ->
        let addr = Int32.of_int addr in
        try
          Memory.store_word_sim m addr w;
          let loaded = Memory.load_word_from_sim m addr in
          Format.asprintf "Addr: %lx, mem: %a, LOADED: %lx, EXPECTED: %lx"
            addr
            Memory.pp m
            loaded
            w
        with _ -> (
            Format.asprintf "Addr: %lx, mem: %a, ERROR READING!"
              addr
              Memory.pp m
          )
      )
    (gen_random_word (Gen.return m))
    (fun (addr, w, m) ->
       let addr = Int32.of_int addr in
       Memory.store_word_sim m addr w;
       w = Memory.load_word_from_sim m addr
    )

let test risc_hello_world test_file =
  let open OUnit2 in
  "memory tests"
  >:::[ random_byte_store_read
      ; random_word_store_read
      ; reference_word_store_read risc_hello_world
      ; reference_word_store_read test_file ]
