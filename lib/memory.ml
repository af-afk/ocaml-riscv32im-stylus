
module Region = struct
  open Bigarray

  type arr = (int, int8_unsigned_elt, c_layout) Bigarray.Array1.t

  let pp_arr _ _ = ()

  type t =
    { desc: string
    ; mem: arr
    ; base: int
    ; size: int
    ; readable: bool
    ; writeable: bool
    ; executable: bool }
  [@@deriving show]

  let create ~desc ~base ~size ~readable ~writeable ~executable =
    { desc
    ; mem = Array1.create Int8_unsigned C_layout size
    ; base
    ; size
    ; readable
    ; writeable
    ; executable }

  let gen =
    let open QCheck2.Gen in
    map
      (fun (desc, base, size, readable, writeable, executable) ->
         create ~desc ~base ~size ~readable ~writeable ~executable)
      (tup6 string int (int_range 0 1024) bool bool bool)

  let to_seq_words { mem ; _ } =
    let module B = Bigarray.Array1 in
    let get = B.get mem in
    let rec loop i () =
      if B.dim mem = i then Seq.Nil
      else
        let b0 = get i in
        let b1 = get (i + 1) in
        let b2 = get (i + 2) in
        let b3 = get (i + 3) in
        Seq.Cons (b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24), loop (i + 4)) in
    loop 0
end

type t = Region.t list [@@deriving show, qcheck2]

let ($$) g f x = g (f x)

let of_path name =
  let total_memory_size = 0x80000000 in (* 2GB *)
  let bfd = Libbinutils.open_obj name in
  let pc =
    match
      Libbinutils.asymbols_seq bfd
      |> Seq.find ((=) "_start" $$ Libbinutils.asymbol_name)
    with
    | Some sym -> Int32.of_int (Libbinutils.asymbol_value sym)
    | None -> invalid_arg "No symbol titled _start" in
  let sections =
    Libbinutils.asections_seq bfd |> Seq.filter_map (fun sect ->
        let name = Libbinutils.section_name sect in
        let vma = Libbinutils.get_section_vma sect in
        let size = Libbinutils.get_section_size sect in
        let Libbinutils.Perms.{ alloc; load; readonly; code; data; rom; _ } =
          Libbinutils.section_perms sect in
        if alloc then
          Some (name, vma, size, load, readonly, code, data, rom)
        else None
      ) |> List.of_seq in
  (* Find the bounds of the program: *)
  let program_start =
    match sections with
    | [] -> 0x80000000 (* Default if nothing found *)
    | (_, vma, _, _, _, _, _, _) :: _ ->
      sections
      |> List.map (fun (_, vma, _, _, _, _, _, _) -> vma)
      |> List.fold_left min vma in
  (* Calculate total memory needed: from program start to 2GB *)
  let memory_start = program_start in
  let memory_end = memory_start + total_memory_size in
  let total_size = memory_end - memory_start in
  (* Create one large memory region. *)
  let large_memory =
    Bigarray.Array1.create Bigarray.Int8_unsigned Bigarray.C_layout total_size in
  Bigarray.Array1.fill large_memory 0; (* Zero out the extra region *)
  (* Load section contents into the appropriate offsets *)
  List.iter (fun (name, vma, size, load, _, _, _, _) ->
      if size > 0 then (
        let offset = vma - memory_start in
        if load then (
          (* Load section contents from file *)
          let section =
            Libbinutils.asections_seq bfd
            |> Seq.find (fun s -> Libbinutils.section_name s = name) in
          match section with
          | Some sect ->
            let contents = Libbinutils.get_section_contents bfd sect 0 in
            let src_len = Bigarray.Array1.dim contents in
            let copy_len = min src_len size in
            for i = 0 to copy_len - 1 do
              large_memory.{offset + i} <- contents.{i}
            done;
          | None -> failwith "Could not find section"
        )
      )
    ) sections;
  let memory_region =
  Region.
                        { desc = "unified_memory"
                        ; mem = large_memory
                        ; base = memory_start
                        ; size = total_size
                        ; readable = true   (* Make everything readable *)
                        ; writeable = true  (* Make everything writeable for simplicity *)
                        ; executable = true (* Make everything executable for simplicity *)
                        } in

  (* Stack top at the end of memory, minus a small guard *)
  let stack_guard = 0x1000 in (* 4KB guard *)
  let stack_top = memory_end - stack_guard in

  [memory_region], stack_top, pc

[@@inline always]
let convert_to_int x =
  Option.get (Int32.unsigned_to_int x) (* TODO *)

let find_region regions addr =
  List.find_opt (fun Region.{ base ; size ; _ } ->
      addr >= base && addr < base + size
    ) regions

let ($$) g f x = g (f x)

let load_byte regions addr =
  match find_region regions addr with
  | Some { mem ; base ; readable ; _ } when readable ->
    let offset = addr - base in
    Int32.of_int (Bigarray.Array1.get mem offset land 0xff)
  | Some _ -> failwith "Memory not readable"
  | None -> failwith (Printf.sprintf "Unmapped memory access addr: %x" addr)

let load_byte_sim regions =
  load_byte regions $$ convert_to_int

let load_into_str regions addr len =
  String.init (convert_to_int len) (
    char_of_int
    $$ convert_to_int
    $$ load_byte regions
    $$ (+) addr)

let load_into_str_sim regions addr len =
  load_into_str regions (convert_to_int addr) len

let load_into_array regions addr len =
  Array.init (convert_to_int len) (
    convert_to_int
    $$ load_byte regions
    $$ (+) addr)

let load_byte_unsigned regions addr =
  match find_region regions addr with
  | Some { mem; base; readable; _ } when readable ->
    let offset = addr - base in
    Int32.of_int (Bigarray.Array1.get mem offset land 0xff)
  | Some _ -> failwith "Memory not readable"
  | None -> failwith (Printf.sprintf "Unmapped memory access addr: %x" addr)

let load_byte_unsigned_sim regions =
  load_byte_unsigned regions $$ convert_to_int

let sign_extend_16 x =
  let open Int32 in
  let x = logand x 0xffffl in
  if logand x 0x8000l <> 0l then
    logor x (lognot 0xffffl)
  else x

let load_halfword regions addr =
  let b0 = load_byte_unsigned regions addr in
  let b1 = load_byte_unsigned regions (addr + 1) in
  sign_extend_16 (Int32.logor b0 (Int32.shift_left b1 8))

let load_halfword_from_sim regions =
  load_halfword regions $$ convert_to_int

let load_halfword_unsigned regions addr =
  let b0 = load_byte_unsigned regions addr in
  let b1 = load_byte_unsigned regions (addr + 1) in
  Int32.logor b0 (Int32.shift_left b1 8)

let load_halfword_unsigned_from_sim regions =
  load_halfword_unsigned regions $$ convert_to_int

let load_word regions addr =
  let h0 = load_halfword_unsigned regions addr in
  let h1 = load_halfword_unsigned regions (addr + 2) in
  Int32.logor h0 (Int32.shift_left h1 16)

let load_word_from_sim regions =
  load_word regions $$ convert_to_int

let store_byte regions addr value =
  match find_region regions addr with
  | Some { base ; mem ; writeable ; _ } when writeable ->
    let offset = addr - base in
    Bigarray.Array1.set mem offset (convert_to_int value land 0xff)
  | Some _ -> failwith (Printf.sprintf "Memory offset %x not writeable with byte" addr)
  | None -> failwith (Printf.sprintf "Unmapped memory access addr: %x" addr)

let store_byte_sim regions addr value =
  store_byte regions (convert_to_int addr) value

let store_array regions pos arr =
  for i = 0 to Array.length arr - 1 do
    store_byte regions (pos + i) (Array.get arr i)
  done

let store_halfword regions addr value =
  store_byte regions addr (Int32.logand value 0xffl);
  store_byte regions
    (addr + 1)
    (Int32.logand (Int32.shift_right_logical value 8) 0xffl)

let store_halfword_sim regions addr value =
  store_halfword regions (convert_to_int addr) value

let store_word regions addr value =
  store_halfword regions addr (Int32.logand value 0xffffl);
  store_halfword regions
    (addr + 2)
    (Int32.logand (Int32.shift_right_logical value 16) 0xffffl)

let store_word_sim regions addr value =
  store_word regions (convert_to_int addr) value
