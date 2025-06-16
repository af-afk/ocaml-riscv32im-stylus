
open OUnit2

let store_load = "Store and load" >:: fun _ -> ()

let test =
 "Test Ethereum-specific operations" >::: [ store_load ]
