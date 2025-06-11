
open Riscv32im_stylus

let () =
  let open OUnit2 in
  let bfd1, risc_hello_world, stack_top, pc = Memory.of_path "risc-hello-world" in
  let bfd2, test_file, _, _ = Memory.of_path "test_riscv32im.exe" in
  at_exit (fun () ->
      Libbinutils.close bfd1;
      Libbinutils.close bfd2
    );
  run_test_tt_main (
    "Test the riscv32im simulator"
    >:::[ Test_encoding.test
        ; Test_memory.test risc_hello_world test_file
        ; Test_opcodes.test risc_hello_world stack_top pc ]
  )
