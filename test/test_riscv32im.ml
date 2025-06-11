
open Riscv32im_stylus

let test_encode_decode =
  let open QCheck2 in
  QCheck_ounit.to_ounit2_test @@ Test.make
    ~name:"encode/decode"
    ~print:(fun t ->
        let encoded = Encoding.encode t in
        let decoded = Lifted.from_word 0l (Int32.to_int encoded) in
        Format.asprintf "Encoded word: 0x%lx, decoded word: %a, was actually: %a"
          encoded Lifted.pp decoded Lifted.pp t
      )
    Lifted.gen (fun t ->
        Lifted.equal t (Lifted.from_word 0l (Int32.to_int (Encoding.encode t)))
      )

let gen_random_word m =
  let open QCheck2.Gen in
  let* m = m in
  let min_addr = List.fold_left (fun acc Memory.Region.{ base; _ } ->
    min acc base) Int.max_int m in
  let max_addr = List.fold_left (fun acc Memory.Region.{ base; size; _ } ->
    max acc (base + size - 1)) Int.min_int m in
  let* addr = int_range min_addr (max_addr - 3) in
  let* word_val = int32 in
  return (addr, word_val, m)

let test_random_byte_store_read_internal_conv =
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

let test_random_word_store_read_internal_conv =
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

let test_reference_word_store_read_internal_conv f =
  let open QCheck2 in
  let m, _, _ = Memory.of_path f in
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

let () =
  let open OUnit2 in
  run_test_tt_main (
    "tests" >:::[
      test_encode_decode
    ; test_random_byte_store_read_internal_conv
    ; test_random_word_store_read_internal_conv
    ; test_reference_word_store_read_internal_conv "risc-hello-world"
    ; test_reference_word_store_read_internal_conv "test_riscv32im.exe" ])
