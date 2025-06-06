
open Riscv32im_stylus

let () =
  let mem, _, _ = Memory.of_path (Array.get Sys.argv 1) in
  let pc = ref 0l in
  List.find (fun Memory.Region.{ desc ; _ } -> desc = ".text") mem
  |> Memory.Region.to_seq_words
  |> Seq.iter (fun word ->
      let t = Lifted.from_word !pc word in
      Format.printf "%a@." Lifted.pp_objdump t;
      pc := Int32.add !pc 4l
    )
