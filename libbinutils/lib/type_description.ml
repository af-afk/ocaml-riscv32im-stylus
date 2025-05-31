open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

  module Section = struct
    type t
    let t : t structure typ = structure "bfd_section"
    let name = field t "name" string
    let size = field t "size" ulong
    let next = field t "next" (ptr_opt t)
    let vma = field t "vma" ulong
    let flags = field t "flags" ulong
    let () = seal t
  end

  module Flag = struct
    let alloc_const = constant "SEC_ALLOC" ulong
    let load_const = constant "SEC_LOAD" ulong
    let reloc_const = constant "SEC_RELOC" ulong
    let readonly_const = constant "SEC_READONLY" ulong
    let code_const = constant "SEC_CODE" ulong
    let data_const = constant "SEC_DATA" ulong
    let rom_const = constant "SEC_ROM" ulong

    let alloc = Unsigned.ULong.of_int 0x1
    let load = Unsigned.ULong.of_int 0x2
    let reloc = Unsigned.ULong.of_int 0x4
    let readonly = Unsigned.ULong.of_int 0x8
    let code = Unsigned.ULong.of_int 0x10
    let data = Unsigned.ULong.of_int 0x20
    let rom = Unsigned.ULong.of_int 0x40

    let has t f = Unsigned.ULong.(logand t f <> zero)
  end

  module Bfd = struct
    type t
    let t: t structure typ = structure "bfd"
    let sections = field t "sections" (ptr_opt Section.t)
    let () = seal t
  end

  module Bfd_format = struct
    type t =
      | Unknown
      | Object
      | Archive
      | Core
      | Type_end

    let t =
      enum ~typedef:true "bfd_format"
        [ Unknown, constant "bfd_unknown" int64_t
        ; Object, constant "bfd_object" int64_t
        ; Archive, constant "bfd_archive" int64_t
        ; Core, constant "bfd_core" int64_t
        ; Type_end, constant "bfd_type_end" int64_t ]
  end
end
