
open Riscv32im_stylus

let () =
  let bfd, mem, _, _ = Memory.of_path (Array.get Sys.argv 1) in
  at_exit (fun () -> Libbinutils.close bfd);
  let region = List.find (fun Memory.Region.{ desc ; _ } -> desc = ".text") mem in
  let Memory.Region.{ base ; _ } = region in
  let pc = ref base in
  region
  |> Memory.Region.to_seq_words
  |> Seq.iter (fun word ->
      let t = Lifted.from_word (Int32.of_int !pc) word in
      Format.printf "%x:	%x	%a@." !pc word Lifted.pp_objdump t;
      pc := !pc + 4
    )
