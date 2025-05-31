open Ctypes

module Types = Types_generated
module Functions = Function_description.Functions (Libbfd__c_generated_functions__Function_description__Functions)

open Types

let init = Functions.bfd_init

let open_obj n =
  let b = Functions.bfd_openr n None in
  if Ctypes.is_null b then invalid_arg "Bfd null ptr";
  if not (Functions.bfd_check_format b Types.Bfd_format.Object) then (
    Functions.bfd_close b;
    invalid_arg "Bfd not object"
  );
  b

let asections_seq bfd =
  let rec loop ptr () = match ptr with
    | None -> Seq.Nil
    | Some p -> Seq.Cons (!@p, loop (getf !@p Section.next))
  in
  loop (getf !@bfd Bfd.sections)

let section_name sect = getf sect Section.name
let section_vma sect = getf sect Section.vma
let section_size sect = getf sect Section.size

let ($$) g f x = g (f x)

(* This should be fine to convert since we're in a 32 bit machine! *)

let get_section_size = Unsigned.ULong.to_int $$ section_size

let get_section_vma = Unsigned.ULong.to_int $$ section_vma

module Perms = struct
  type t =
    { alloc: bool
    ; load: bool
    ; reloc: bool
    ; readonly: bool
    ; code: bool
    ; data: bool
    ; rom: bool }
  [@@deriving show]

  let of_flags f =
    let module F = Flag in
    let h x = F.has f x in
    { alloc = h F.alloc
    ; load = h F.load
    ; reloc = h F.reloc
    ; readonly = h F.readonly
    ; code = h F.code
    ; data = h F.data
    ; rom = h F.rom }
end

let section_perms sect = Perms.of_flags (getf sect Section.flags)

let get_section_contents bfd sect offset =
  (*
   * We allocate this on the heap so that this has a shortlived life,
   * without us managing the mmap'd pointer ourselves. Hopefully in our
   * context (Rust binaries that should run on-chain eventually) this is
   * manageable.
   *)
  let offset = Unsigned.ULong.of_int offset in
  let size = section_size sect in
  let mapped =
    Bigarray.(Array1.create int8_unsigned c_layout (Unsigned.ULong.to_int size)) in
  let rc = Functions.bfd_get_section_contents bfd (addr sect)
      (to_voidp (bigarray_start array1 mapped))
      offset
      size in
  if not rc then
    failwith "Bigarray bfd_get_section fail";
  mapped
