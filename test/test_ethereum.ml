
open OUnit2

let store_load =
  q2o @@ QCheck2.Test.make
    ~name:"Store load"
    ~print:sprint_gen_i_registers_vals
    (gen_i_registers_and_values `Andi)
    (fun { x ; y ; res ; _ } -> res = Int32.logand x y)

let test =
 "Test Ethereum-specific operations" >::: [ store_load ]
