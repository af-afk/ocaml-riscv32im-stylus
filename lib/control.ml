
exception Exited of [`Ebreak]

let exit c = raise (Exited c)

let exit_ebreak () = raise (Exited `Ebreak)
