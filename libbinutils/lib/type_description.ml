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
  end
  let () = seal Section.t

  module Bfd = struct
    type t
    let t: t structure typ = structure "bfd"
    let sections = field t "sections" (ptr_opt Section.t)
  end
  let () = seal Bfd.t

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
