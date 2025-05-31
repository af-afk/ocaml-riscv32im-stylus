
module Region = struct
  type arr = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  type t =
    { mem: arr
    ; base: int
    ; size: int
    ; readable: bool
    ; writeable: bool
    ; executable: bool }
end

type t = Region.t list

let of_path name =
  let bfd = Libbinutils.open_obj name in
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
  |> List.of_seq
let find_region regions addr =
  List.find_opt (fun region ->
    addr >= Int32.of_int region.Region.base &&
    addr < Int32.add (Int32.of_int region.Region.base) (Int32.of_int region.Region.size)
  ) regions

let load_byte regions addr =
  match find_region regions addr with
  | Some region when region.readable ->
    let offset = Int32.to_int addr - region.base in
    Int32.of_int (Bigarray.Array1.get region.mem offset)
  | Some _ -> failwith "Memory not readable"
  | None -> failwith "Unmapped memory access"

let load_byte_unsigned regions addr = load_byte regions addr

let load_halfword regions addr =
  let b0 = load_byte regions addr in
  let b1 = load_byte regions (Int32.add addr 1l) in
  Int32.logor b0 (Int32.shift_left b1 8)

let load_halfword_unsigned regions addr = load_halfword regions addr

let load_word regions addr =
  let h0 = load_halfword regions addr in
  let h1 = load_halfword regions (Int32.add addr 2l) in
  Int32.logor h0 (Int32.shift_left h1 16)

let store_byte regions addr value =
  match find_region regions addr with
  | Some region when region.writeable ->
    let offset = Int32.to_int addr - region.base in
    Bigarray.Array1.set region.mem offset (Int32.to_int value)
  | Some _ -> failwith "Memory not writeable"
  | None -> failwith "Unmapped memory access"

let store_halfword regions addr value =
  store_byte regions addr (Int32.logand value 0xffl);
  store_byte regions (Int32.add addr 1l) (Int32.logand (Int32.shift_right_logical value 8) 0xffl)

let store_word regions addr value =
  store_halfword regions addr (Int32.logand value 0xffffl);
  store_halfword regions (Int32.add addr 2l) (Int32.logand (Int32.shift_right_logical value 16) 0xffffl)
