
open Riscv32im_stylus

let max_cycles = 10

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
      | item :: rest -> item :: update_list rest in
    let updated = update_list t in
    let rec take n = function
      | [] -> []
      | x :: xs when n > 0 -> x :: take (n - 1) xs
      | _ -> [] in
    take max_cycles updated
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
  let bfd, mem, stack_top, pc = Memory.of_path (Array.get Sys.argv 1) in
  at_exit (fun () -> Libbinutils.close bfd);
  let registers = { Registers.empty_spike with t_r_sp = Int32.of_int stack_top } in
  let module Circ_registers = CIRCULAR_BUFFER(Registers) in
  let module Circ_hex = CIRCULAR_BUFFER(struct
      include Int32
      let empty = 0l
      let pp fmt t = Format.fprintf fmt "%lx" t
    end) in
  let module Circ_lifted = CIRCULAR_BUFFER(Lifted) in
  let circ_registers = Circ_registers.create () in
  let circ_words = Circ_hex.create () in
  let circ_lifted = Circ_lifted.create () in
  let circ_pc = Circ_hex.create () in
  let count = ref 0 in
  let sim = ref (Simulator.make ~b:mem ~r:registers ~pc ()) in
  let cycles = ref Cycle_detection.empty in
  let last_ops = ref [] in
  let print_cleanup _ = (
    Simulator.pp fmt_stderr !sim;
    (* TODO make cleaner *)
    Format.fprintf fmt_stderr "@.CYCLE DETECTION: %a"
      Cycle_detection.pp !cycles;
    Format.fprintf fmt_stderr "@.REGISTERS CIRCULAR BUFFER: %a"
      Circ_registers.pp circ_registers;
    Format.fprintf fmt_stderr "@.WORDS CIRCULAR BUFFER: %a"
      Circ_hex.pp circ_words;
    Format.fprintf fmt_stderr "@.LIFTED CIRCULAR BUFFER: %a"
      Circ_lifted.pp circ_lifted;
    Format.fprintf fmt_stderr "@.PC CIRCULAR BUFFER: %a"
      Circ_hex.pp circ_pc;
    Format.fprintf fmt_stderr "@.CURRENT STACK POINTER: %lx@.INSTRUCTION COUNT: %l" !sim.pc !count;
    Format.fprintf fmt_stderr "@.UNIQUE OP NAMES SEEN: %a@." (Format.pp_print_list Lifted.pp) !last_ops;
  ) in
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle print_cleanup);
  try
    while true do
      let Simulator.{ pc; r; last_op ; _ } = !sim in
      Circ_registers.push circ_registers r;
      cycles := Cycle_detection.track !cycles pc r;
      Circ_hex.push circ_pc pc;
      sim := Simulator.step
          ~before_lift:(fun x -> Circ_hex.push circ_words x; x)
          ~after_lift:(fun x -> Circ_lifted.push circ_lifted x; x)
          ~fmt:fmt_stderr
          !sim;
      match last_op with
      | Some op ->
        last_ops :=
          List.sort_uniq Helpers.compare_lifted_name (op :: !last_ops)
      | None -> ();
        Format.pp_force_newline fmt_stderr ();
        incr count
    done
  with
  | Control.Exited _ -> ()
  | err -> (
      print_cleanup 1;
      raise err
    )
