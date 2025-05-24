(lang dune 3.18)

(name riscv32im)

(generate_opam_files true)

(source
 (github af-afk/riscvim32))

(authors "Bayge <alex@fluidity.money>")

(license MIT)

(package
 (name riscv32im)
 (synopsis "Simulates the RISCV32IM ELF files.")
 (depends ocaml)
 (tags
  (riscv riscv32 riscv32im ocaml stylus)))
