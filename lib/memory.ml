
module Region = struct
  type arr = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  type t =
    { desc: string
    ; mem: arr
    ; base: int32
    ; size: int32
    ; readable: bool
    ; writeable: bool
    ; executable: bool }

  let create ~desc ~base ~size ~readable ~writeable ~executable =
    { desc
    ; mem =
        Bigarray.Array1.create
          Bigarray.Int8_unsigned
          Bigarray.C_layout
          (Option.get (Int32.unsigned_to_int size))
    ; base
    ; size
    ; readable
    ; writeable
    ; executable }

  let pp fmt { desc ; base ; size ; readable ; writeable ; executable ; _ } =
    Format.fprintf fmt "{ desc = %s; base = %ld (0x%lx); size = %ld (0x%lx); readable = %b; writeable = %b; executable = %b }"
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
end

type t = Region.t list [@@deriving show]

let ($$) g f x = g (f x)

let of_path name =
  let ram_start = 0x80000000l in
  let ram_size = 0x08000000l in (* 128 MB *)
  let ram_end = Int32.add ram_start ram_size in
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
        if alloc && load then
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
  let ram_region = Region.create
      ~desc:"ram"
      ~base:ram_start
      ~size:ram_size
      ~readable:true
      ~writeable:true
      ~executable:true in
  let stack_top = Int32.sub ram_end 0x100l in
  (sections @ [ram_region]), stack_top, pc

let find_region regions addr =
  List.find_opt (fun Region.{ base ; size ; _ } ->
      addr >= base &&
      addr < Int32.add base size
    ) regions

let ($$) g f x = g (f x)

let load_byte regions addr =
  match find_region regions addr with
  | Some { mem ; base ; readable ; _ } when readable ->
    let offset = Int32.sub addr base in
    let offset_int = Int32.to_int offset in
    Int32.of_int (Bigarray.Array1.get mem offset_int land 0xFF)
  | Some _ -> failwith "Memory not readable"
  | None -> failwith (Printf.sprintf "Unmapped memory access addr: %ld" addr)

let load_into_str regions addr len =
  String.init (Int32.to_int len) (
    char_of_int
    $$ Int32.to_int
    $$ load_byte regions
    $$ Int32.add addr
    $$ Int32.of_int)

let load_into_array regions addr len =
  Array.init (Int32.to_int len) (
    Int32.to_int
    $$ load_byte regions
    $$ Int32.add addr
    $$ Int32.of_int)

let load_byte_unsigned regions addr =
  match find_region regions addr with
  | Some { mem; base; readable; _ } when readable ->
    let offset = Int32.to_int (Int32.sub addr base) in
    Int32.of_int (Bigarray.Array1.get mem offset land 0xff)
  | Some _ -> failwith "Memory not readable"
  | None -> failwith (Printf.sprintf "Unmapped memory access addr: %ld" addr)

let sign_extend_16 x =
  let open Int32 in
  let x = logand x 0xffffl in
  if logand x 0x8000l <> 0l then
    logor x (lognot 0xffffl)
  else x

let load_halfword regions addr =
  let b0 = load_byte_unsigned regions addr in
  let b1 = load_byte_unsigned regions (Int32.add addr 1l) in
  sign_extend_16 (Int32.logor b0 (Int32.shift_left b1 8))

let load_halfword_unsigned regions addr =
  let b0 = load_byte_unsigned regions addr in
  let b1 = load_byte_unsigned regions (Int32.add addr 1l) in
  Int32.logor b0 (Int32.shift_left b1 8)

let load_word regions addr =
  let h0 = load_halfword_unsigned regions addr in
  let h1 = load_halfword_unsigned regions (Int32.add addr 2l) in
  Int32.logor h0 (Int32.shift_left h1 16)

let store_byte regions addr value =
  match find_region regions addr with
  | Some { base ; mem ; writeable ; _ } when writeable ->
    let offset = Int32.sub addr base in
    let offset_int = Int32.to_int offset in
    Bigarray.Array1.set mem offset_int (Int32.to_int value land 0xFF)
  | Some _ -> failwith (Printf.sprintf "Memory offset %ld not writeable with byte" addr)
  | None -> failwith (Printf.sprintf "Unmapped memory access addr: %ld" addr)

let store_array regions pos arr =
  for i = 0 to Array.length arr - 1 do
    store_byte regions (Int32.(add pos (of_int i))) (Array.get arr i)
  done

let store_halfword regions addr value =
  store_byte regions addr (Int32.logand value 0xffl);
  store_byte regions
    (Int32.add addr 1l)
    (Int32.logand (Int32.shift_right_logical value 8) 0xffl)

let store_word regions addr value =
  store_halfword regions addr (Int32.logand value 0xffffl);
  store_halfword regions
    (Int32.add addr 2l)
    (Int32.logand (Int32.shift_right_logical value 16) 0xffffl)
