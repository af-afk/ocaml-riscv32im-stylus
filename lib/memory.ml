
module Region = struct
  type arr = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  type t =
    { mem: arr
    ; base: int32
    ; size: int32
    ; readable: bool
    ; writeable: bool
    ; executable: bool }

  let create ~base ~size ~readable ~writeable ~executable =
    { mem = Bigarray.Array1.create Bigarray.Int8_unsigned Bigarray.C_layout (Int32.to_int size)
    ; base
    ; size
    ; readable
    ; writeable
    ; executable }
end

type t = Region.t list

let of_path name =
  let bfd = Libbinutils.open_obj name in
  let sections =
    Libbinutils.asections_seq bfd |> Seq.filter_map (fun sect ->
        let vma = Libbinutils.get_section_vma sect in
        let contents = Libbinutils.get_section_contents bfd sect 0 in
        let size = Libbinutils.get_section_size sect in
        (* We ignore reloc! *)
        let Libbinutils.Perms.{ alloc ; load ; readonly ; code ; data ; rom; _ } =
          Libbinutils.section_perms sect in
        if alloc && load then
          let readable = true in (* We're always going to leave this like this! *)
          let writeable = (not readonly) && (not rom) in
          let executable = code && (not data) in
          Some (Region.
                  { mem = contents
                  ; base = vma
                  ; size = size
                  ; readable
                  ; writeable
                  ; executable })
        else
          None
      )
    |> List.of_seq in
  let elf_end = List.fold_left (fun max_end region ->
      let region_end = Int32.add region.Region.base region.Region.size in
      max max_end region_end
    ) 0x80000000l sections in
  let user_base =
    Int32.logand (Int32.add elf_end 0xFFFFl) (Int32.lognot 0xFFFFl) in
  let ram_end = Int32.add 0x80000000l 0x8000000l in
  let user_size = Int32.sub ram_end user_base in
  let user_region = Region.create
      ~base:user_base
      ~size:user_size
      ~readable:true
      ~writeable:true
      ~executable:false in
  let sections = sections @ [user_region] in
  let stack_top = Int32.sub ram_end 0x100l in
  (sections, stack_top)

let find_region regions addr =
  List.find_opt (fun Region.{ base ; size ; _ } ->
      addr >= base &&
      addr < Int32.add base size
    ) regions

let load_byte regions addr =
  match find_region regions addr with
  | Some { mem ; base ; readable ; _ } when readable ->
    let offset = Int32.sub addr base in
    let offset_int = Int32.to_int offset in
    Int32.of_int (Bigarray.Array1.get mem offset_int land 0xFF)
  | Some _ -> failwith "Memory not readable"
  | None -> failwith (Printf.sprintf "Unmapped memory access addr: %ld" addr)

let load_byte_unsigned regions addr = load_byte regions addr

let load_halfword regions addr =
  let b0 = load_byte regions addr in
  let b1 = load_byte regions (Int32.add addr 1l) in
  Int32.logor b0 (Int32.shift_left b1 8)

let load_halfword_unsigned regions addr = load_halfword regions addr

let load_word regions addr =
  let b0 = load_byte regions addr in
  let b1 = load_byte regions (Int32.add addr 1l) in
  let b2 = load_byte regions (Int32.add addr 2l) in
  let b3 = load_byte regions (Int32.add addr 3l) in
  Printf.eprintf "Bytes at 0x%08lx: %02lx %02lx %02lx %02lx\n"
    addr b0 b1 b2 b3;
  let h0 = load_halfword regions addr in
  let h1 = load_halfword regions (Int32.add addr 2l) in
  let word = Int32.logor h0 (Int32.shift_left h1 16) in
  Printf.eprintf "Word: 0x%08lx\n" word;
  word

let store_byte regions addr value =
  match find_region regions addr with
  | Some { base ; mem ; writeable ; _ } when writeable ->
    let offset = Int32.sub addr base in
    let offset_int = Int32.to_int offset in
    Bigarray.Array1.set mem offset_int (Int32.to_int value)
  | Some _ -> failwith "Memory not writeable"
  | None -> failwith (Printf.sprintf "Unmapped memory access addr: %ld" addr)

let store_halfword regions addr value =
  store_byte regions addr (Int32.logand value 0xffl);
  store_byte regions (Int32.add addr 1l) (Int32.logand (Int32.shift_right_logical value 8) 0xffl)

let store_word regions addr value =
  store_halfword regions addr (Int32.logand value 0xffffl);
  store_halfword regions (Int32.add addr 2l) (Int32.logand (Int32.shift_right_logical value 16) 0xffffl)
