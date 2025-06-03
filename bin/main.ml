
open Riscv32im_stylus

let max_cycles = 1000

module Cycle_detection = struct
  type t = ((int32 * Registers.t) * int) list [@@deriving show]

  let empty: t = []

  let track t pc r =
    let key = (pc, r) in
    let rec update_list = function
      | [] -> [(key, 1)]
      | (k, count) :: rest when k = key ->
        if count + 1 > max_cycles then (
          Registers.pp Format.err_formatter r;
          Format.(pp_force_newline err_formatter ());
          failwith "Cycle detected!"
        )
        else (key, count + 1) :: rest
      | item :: rest -> item :: update_list rest
    in
    let updated = update_list t in
    let rec take n = function
      | [] -> []
      | x :: xs when n > 0 -> x :: take (n - 1) xs
      | _ -> []
    in
    take 10 updated
end

module type Circular_buffer_s = sig
  type t
  val pp: Format.formatter -> t -> unit
  val empty: t
end

module CIRCULAR_BUFFER (S: Circular_buffer_s) = struct
  type s = { pos: int; t: S.t array }
  type t = s ref

  let create: unit -> t = fun () -> ref { pos = 0; t = Array.make 10 S.empty }

  let push state v =
    let { pos; t } = !state in
    let pos = if pos = Array.length t - 1 then 0 else pos + 1 in
    Array.set t pos v;
    state := { !state with pos }

  let pp fmt state =
    let { pos; t } = !state in
    let len = Array.length t in
    Format.fprintf fmt "[";
    for i = 1 to len do
      let idx = (pos + i) mod len in
      Format.fprintf fmt "%a%s" S.pp t.(idx) (if i < len then "; " else "")
    done;
    Format.fprintf fmt "]"
end

let fmt_stderr = Format.err_formatter

let () =
  let mem, stack_top, pc = Memory.of_path (Array.get Sys.argv 1) in
  let registers = { Registers.empty_spike with t_r_sp = stack_top } in
  let module Circ_registers = CIRCULAR_BUFFER(Registers) in
  let module Circ_words = CIRCULAR_BUFFER(struct
      include Int32
      let empty = 0l
      let pp fmt t = Format.fprintf fmt "%ld" t
    end) in
  let module Circ_lifted = CIRCULAR_BUFFER(Lifted) in
  let circ_registers = Circ_registers.create () in
  let circ_words = Circ_words.create () in
  let circ_lifted = Circ_lifted.create () in
  let sim = ref (
      Simulator.make
        ~b:mem
        ~r:registers
        ~pc
        ~e:Ethereum.empty
        ~cd_b:Calldata.empty
        ~rd_b:Calldata.empty) in
  let cycles = ref Cycle_detection.empty in
  let print_cleanup _ = (
    Simulator.pp fmt_stderr !sim;
    Format.fprintf fmt_stderr "\nCYCLE DETECTION:";
    Cycle_detection.pp fmt_stderr !cycles;
    Format.fprintf fmt_stderr "\nREGISTERS CIRCULAR BUFFER:";
    Circ_registers.pp fmt_stderr circ_registers;
    Format.fprintf fmt_stderr "\nWORDS CIRCULAR BUFFER:";
    Circ_words.pp fmt_stderr circ_words;
    Format.fprintf fmt_stderr "\nLIFTED CIRCULAR BUFFER BUFFER:";
    Circ_lifted.pp fmt_stderr circ_lifted;
    Format.pp_force_newline fmt_stderr ()
  ) in
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle print_cleanup);
  try
    while true do
      sim := Simulator.step
          ~before_lift:(fun x -> Circ_words.push circ_words x; x)
          ~after_lift:(fun x -> Circ_lifted.push circ_lifted x; x)
          fmt_stderr
          !sim;
      let Simulator.{ pc; r; _ } = !sim in
      Circ_registers.push circ_registers r;
      cycles := Cycle_detection.track !cycles pc r
    done
  with
    err ->
    print_cleanup 1;
    raise err
