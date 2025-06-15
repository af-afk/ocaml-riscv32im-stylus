
module Region = struct
  type arr = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t =
    { desc: string
    ; mem: arr
    ; base: int
    ; size: int
    ; readable: bool
    ; writeable: bool
    ; executable: bool }

  let create ?(readable = true) ?(writeable = true) ?(executable = true) ~desc ~base ~size () =
    { desc
    ; mem =
        Bigarray.Array1.create
          Bigarray.Int8_unsigned
          Bigarray.C_layout
          size
    ; base
    ; size
    ; readable
    ; writeable
    ; executable }

  let pp fmt { desc ; base ; size ; readable ; writeable ; executable ; _ } =
    Format.fprintf fmt "{ desc = %s; base = %x (0x%x); size = %d (0x%x); readable = %b; writeable = %b; executable = %b }"
      desc base base size size readable writeable executable

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

  let gen =
    let open QCheck2.Gen in
    let* desc = string in
    let* size = int_range 4 1024 in
    let* base = int_range 0 10000 in
    let m = create ~desc ~base ~size () in
    let* _ = fix (fun self i ->
        if i >= size then pure ()
        else
          let* v = int in
          let () = Bigarray.Array1.set m.mem i v in
          self (i + 1)
      ) 0 in
    return m
end

type t = Region.t list [@@deriving show]

let empty: t = []

let gen =
  (* Generate non overlapping, contiguous memory. *)
  let open QCheck2.Gen in
  let* regions = list_size (int_range 1 8) Region.gen in
  let sorted =
    List.sort
      Region.(fun { base = a ; _ } { base = b ; _ } -> compare a b)
      regions in
  let rec make acc next_base = function
    | [] -> return (List.rev acc)
    | r :: rest ->
      let new_region = Region.{ r with base = next_base } in
      make (new_region :: acc) (next_base + r.size) rest in
  make [] 0 sorted

let ($$) g f x = g (f x)

let of_path name =
  let ram_start = 0x80000000 in
  let ram_size = 0x08000000 in (* 128 MB *)
  let ram_end = ram_start + ram_size in
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
        let contents = Libbinutils.get_section_contents bfd sect 0 in
        let size = Libbinutils.get_section_size sect in
        let Libbinutils.Perms.{ alloc; load; readonly; code; data; rom; _ } =
          Libbinutils.section_perms sect in
        if alloc && load && vma >= ram_start && vma < ram_end then
          Some (
            Region.
              { desc = name
              ; mem = contents
              ; base = vma
              ; size = size
              ; readable = true
              ; writeable = (not readonly) && (not rom)
              ; executable = code && (not data) })
        else None
      ) |> List.of_seq in
  let sorted_sections =
    List.sort
      Region.(fun { base = a; _ } { base = b ; _ } -> compare a b)
      sections in
  let rec fill_gaps acc current_addr = function
    | [] ->
      if current_addr < ram_end then
        let gap_region = Region.create
            ~desc:"ram_gap"
            ~base:current_addr
            ~size:(ram_end - current_addr)
            ~readable:true
            ~writeable:true
            ~executable:true
            () in
        List.rev (gap_region :: acc)
      else
        List.rev acc
    | section :: rest ->
      let section_start = section.Region.base in
      let section_end = section_start + section.size in
      let acc_with_gap =
        if current_addr < section_start then
          let gap_region = Region.create
              ~desc:"ram_gap"
              ~base:current_addr
              ~size:(section_start - current_addr)
              ~readable:true
              ~writeable:true
              ~executable:true
              () in
          gap_region :: acc
        else
          acc in
      let acc_with_section = section :: acc_with_gap in
      fill_gaps acc_with_section section_end rest
  in

  let all_regions = fill_gaps [] ram_start sorted_sections in
  let stack_top = ram_end - 0x100 in
  bfd, (all_regions), stack_top, pc

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