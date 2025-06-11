(lang dune 3.18)

(name riscv32im_stylus)

(generate_opam_files true)

(source
 (github af-afk/riscvim32))

(authors "Bayge <alex@fluidity.money>")

(license MIT)

(package
 (name riscv32im_stylus)
 (synopsis
  "Simulates RISCV32IM, works with ELF, and has environment calling for Ethereum features.")
 (depends
  ocaml
   ppx_deriving
   ppx_hash
   ppx_sexp_conv
   ppx_hash
   ppx_compare
   sexplib0
   conf-binutils
   ctypes)
 (tags
  (riscv riscv32 riscv32im ocaml stylus ethereum)))
