
let print_hex_bigarray ba =
  for i = 0 to Bigarray.Array1.dim ba - 1 do
    Printf.eprintf "%02x " (Bigarray.Array1.get ba i)
  done;
  Printf.eprintf "\n"

let () =
  Libbinutils.init ();
  let bfd = Libbinutils.open_obj (Array.get Sys.argv 1) in
  Libbinutils.asymbols_seq bfd |> Seq.iter (fun sym ->
    Printf.eprintf "sym: %s, loc: %d\n"
    (Libbinutils.asymbol_name sym)
    (Libbinutils.asymbol_value sym)
  );
  Libbinutils.asections_seq bfd |> Seq.iter (fun sect ->
      if Libbinutils.section_name sect = ".text" then (
        Format.eprintf "Flags: %s\n" (Libbinutils.Perms.show (Libbinutils.section_perms sect));
        let x = Libbinutils.get_section_contents bfd sect 0 in
        print_hex_bigarray x
      )
    )
