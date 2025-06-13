
open Riscv32im_stylus

let encode_decode =
  let open QCheck2 in
  QCheck_ounit.to_ounit2_test @@ Test.make
    ~name:"Encode/decode"
    ~print:(fun t ->
        let encoded = Encoding.encode t in
        let decoded = Lifted.from_word 0l (Int32.to_int encoded) in
        Format.asprintf "Encoded word: 0x%lx, decoded word: %a, was actually: %a"
          encoded Lifted.pp decoded Lifted.pp t
      )
    Lifted.gen
    (fun t ->
       Lifted.equal t (Lifted.from_word 0l (Int32.to_int (Encoding.encode t)))
    )

let test =
  let open OUnit2 in
  "encoding tests"  >:::[encode_decode]
