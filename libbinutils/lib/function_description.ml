open Ctypes

module Functions (F: Ctypes.FOREIGN) = struct
  open F
  open Types_generated

  let bfd_init = foreign "bfd_init" (void @-> returning void)

  let bfd_openr = foreign "bfd_openr"
      (string @-> string_opt @-> returning (ptr Bfd.t))

  let bfd_check_format = foreign "bfd_check_format"
      ((ptr Bfd.t) @-> Bfd_format.t @-> returning bool)

  let bfd_close = foreign "bfd_close" ((ptr Bfd.t) @-> returning void)

  let bfd_get_section_contents = foreign "bfd_get_section_contents"
      ((ptr Bfd.t) @->
       (ptr Section.t) @->
       (ptr void) @->
       ulong @->
       ulong @->
       (returning bool))
end
