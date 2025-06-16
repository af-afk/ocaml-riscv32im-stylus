
open Riscv32im_stylus

open Storage

open OUnit2

let random_byte_store_read =
  QCheck_ounit.to_ounit2_test @@ QCheck2.Test.make
    ~name:"Store byte/read byte"
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
       assert_equal b (Memory.load_byte_unsigned_sim m addr);
       let exp_byte = Int32.logand b 0x000000ffl in
       let exp_ext = Int32.shift_right (Int32.shift_left exp_byte 24) 24 in
       assert_equal exp_ext (Memory.load_byte m (Int32.to_int addr));
       true
    )

let random_word_store_read =
  QCheck_ounit.to_ounit2_test @@ QCheck2.Test.make
    ~name:"Store word/read word"
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
  let m = List.rev_map (fun t ->
      Memory.Region.{ t with readable = true ; writeable = true }) m in
  QCheck_ounit.to_ounit2_test @@ QCheck2.Test.make
    ~name:"Test binary store word/read word"
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
    (gen_random_word (QCheck2.Gen.return m))
    (fun (addr, w, m) ->
       let addr = Int32.of_int addr in
       Memory.store_word_sim m addr w;
       w = Memory.load_word_from_sim m addr
    )

let test risc_hello_world test_file =
  "Memory tests" >:::[ random_byte_store_read
                     ; random_word_store_read
                     ; reference_word_store_read risc_hello_world
                     ; reference_word_store_read test_file ]
