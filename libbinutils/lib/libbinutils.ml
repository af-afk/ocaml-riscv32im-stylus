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

let iter_asections bfd f =
  let rec loop sect_ptr =
    match sect_ptr with
    | None -> ()
    | Some ptr ->
      let sect = !@ptr in
      f sect;
      loop (getf sect Section.next) in
  loop (getf !@bfd Bfd.sections)

let section_name sect = getf sect Section.name
let section_vma sect = getf sect Section.vma
let section_size sect = getf sect Section.size

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
