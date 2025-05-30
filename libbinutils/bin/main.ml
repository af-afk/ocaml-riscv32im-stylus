
let print_hex_bigarray ba =
  for i = 0 to Bigarray.Array1.dim ba - 1 do
    Printf.eprintf "%02x " (Bigarray.Array1.get ba i)
  done;
  Printf.eprintf "\n"

let () =
  Libbinutils.init ();
  let bfd = Libbinutils.open_obj (Array.get Sys.argv 1) in
  Libbinutils.iter_asections bfd (fun sect ->
      if Libbinutils.section_name sect = ".text" then (
        let x = Libbinutils.get_section_contents bfd sect 0 in
        print_hex_bigarray x
      )
    )
