
open QCheck2.Gen

open Riscv32im_stylus

let gen_random_loc m =
  let* m = m in
  let min_addr = List.fold_left (fun acc Memory.Region.{ base; _ } ->
    min acc base) Int.max_int m in
  let max_addr = List.fold_left (fun acc Memory.Region.{ base; size; _ } ->
    max acc (base + size - 1)) Int.min_int m in
  let* addr = int_range min_addr (max_addr - 3) in
  return (addr, m)

let gen_random_word m =
  let* addr, m = gen_random_loc m in
  let* w = int32 in
  return (addr, w, m)
