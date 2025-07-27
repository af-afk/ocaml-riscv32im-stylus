let apply g f x = g (f x)

let to_int x =
  match Int32.unsigned_to_int x with
  | Some v -> v
  | None -> failwith "Bad int conversion"

let get_a0_a1 Cpu.{ r = Registers.{ t_r_a0; t_r_a1; _ }; _ } = (t_r_a0, t_r_a1)
let get_a0_a1_int = apply (fun (x, y) -> (to_int x, to_int y)) get_a0_a1

let account_balance _ = failwith "Unimplemented account balance"
let account_code _ = failwith "Unimplemented account code"
let account_code_size _ = failwith "Unimplemented codesize"
let account_codehash _ = failwith "Unimplemented codehash"

let ethereum_load t =
  let ptr_key, ptr_write = get_a0_a1_int t in
  let Cpu.{ e_m; b; _ } = t in
  let key = Ethereum_word.of_array (Memory.load_into_array b ptr_key 32l) in
  let v = Ethereum_mem.load_word e_m key in
  Memory.store_array b ptr_write (Ethereum_word.to_array v);
  t

let ethereum_store t =
  let Cpu.{ e_m; b; _ } = t in
  let ptr_key, ptr_val = get_a0_a1_int t in
  let key = Memory.load_into_array b ptr_key 32l in
  let key = Ethereum_word.of_array key in
  let v = Ethereum_word.of_array (Memory.load_into_array b ptr_val 32l) in
  { t with e_m = Ethereum_mem.store_word e_m key v }

let storage_flush_cache t = t

let block_basefee t =
  let Cpu.{ b; r = Registers.{ t_r_a0 = ptr_write; _ }; _ } = t in
  let ptr_write = to_int ptr_write in
  (* At the time of writing, this is the base fee on Superposition Mainnet. *)
  let base_fee = 200000000 in
  Memory.store_array b ptr_write Ethereum_word.(to_array (of_int base_fee));
  t

let chainid t =
  let Cpu.{ b; r = Registers.{ t_r_a0 = ptr_write; _ }; _ } = t in
  let ptr_write = to_int ptr_write in
  let chainid = 55244 in
  Memory.store_array b ptr_write Ethereum_word.(to_array (of_int chainid));
  t

let block_coinbase _ = failwith "Unimplemented coinbase"
let block_gas_limit _ = failwith "Unimplemented gas limit"
let block_number _ = failwith "Unimplemented number"
let block_timestamp _ = failwith "Unimplemented timestamp"
let call_contract _ = failwith "Unimplemented call contract"
let contract_address _ = failwith "Unimplemented contract address"
let create1 _ = failwith "Unimplemented create1"
let create2 _ = failwith "Unimplemented create2"
let delegate_call_contract _ = failwith "Unimplemented delegatecall"
let emit_log _ = failwith "Unimplemented emit log"
let evm_gas_left _ = failwith "Unimplemented evm gas left"
let evm_ink_left _ = failwith "Unimplemented evm ink left"

let pay_for_memory_grow t = t

let msg_reentrant t =
  let Cpu.{ r; _ } = t in
  { t with r = Registers.update r `A0 0l }

let msg_sender _ = failwith "Unimplemented msg sender"

let msg_value t =
  let Cpu.{ b; r = Registers.{ t_r_a0 = ptr_write; _ }; _ } = t in
  let ptr_write = to_int ptr_write in
  Memory.store_array b ptr_write Ethereum_word.(to_array zero);
  t

let native_keccak256 _ = failwith "Unimplemented native keccak"

let read_args t =
  let Cpu.{ b; e_cd; r = Registers.{ t_r_a0 = ptr_write; _ }; _ } = t in
  Memory.store_array b (to_int ptr_write) e_cd;
  t

let read_return_data _ = failwith "Unimplemented read return data"

let write_result t =
  let Cpu.{ b; r = Registers.{ t_r_a0 = ptr_read; t_r_a1 = len ; _ }; _ } = t in
  let rd = Memory.load_into_array b (to_int ptr_read) len in
  { t with e_rd = Array.map Int32.of_int rd }

let return_data_size _ = failwith "Unimplemented return data size"
let static_call_contract _ = failwith "Unimplemented static call contract"
let tx_gas_price _ = failwith "Unimplemented tx gas price"
let tx_ink_price _ = failwith "Unimplemented tx ink price"
let tx_origin _ = failwith "Unimplemented tx origin"

let args_len t =
  let Cpu.{ e_cd ; r; _ } = t in
  { t with r = Registers.update r `A0 (Int32.of_int (Array.length e_cd)) }

let console fmt t =
  let from, length = get_a0_a1 t in
  let Cpu.{ b; _ } = t in
  Format.fprintf fmt "%s@." (Memory.load_into_str_sim b from length);
  t

type op =
  | Account_balance
  | Account_code
  | Account_code_size
  | Account_codehash
  | Ethereum_load
  | Ethereum_store
  | Storage_flush_cache
  | Block_basefee
  | Chainid
  | Block_coinbase
  | Block_gas_limit
  | Block_number
  | Block_timestamp
  | Call_contract
  | Contract_address
  | Create1
  | Create2
  | Delegate_call_contract
  | Emit_log
  | Evm_gas_left
  | Evm_ink_left
  | Pay_for_memory_grow
  | Msg_reentrant
  | Msg_sender
  | Msg_value
  | Native_keccak256
  | Read_args
  | Read_return_data
  | Write_result
  | Return_data_size
  | Static_call_contract
  | Tx_gas_price
  | Tx_ink_price
  | Tx_origin
  | Args_len
  | Console
[@@deriving show, eq, compare, enum]

let bump_pc t = Cpu.{ t with pc = Int32.add 4l t.pc }

let ecall fmt t =
  let Cpu.{ r = Registers.{ t_r_a7 = no; _ }; _ } = t in
  let op =
    match op_of_enum (Int32.to_int no) with
    | Some v -> v
    | None -> failwith "Bad ecall"
  in
  bump_pc
    (match op with
    | Account_balance -> account_balance t
    | Account_code -> account_code t
    | Account_code_size -> account_code_size t
    | Account_codehash -> account_codehash t
    | Ethereum_load -> ethereum_load t
    | Ethereum_store -> ethereum_store t
    | Storage_flush_cache -> storage_flush_cache t
    | Block_basefee -> block_basefee t
    | Chainid -> chainid t
    | Block_coinbase -> block_coinbase t
    | Block_gas_limit -> block_gas_limit t
    | Block_number -> block_number t
    | Block_timestamp -> block_timestamp t
    | Call_contract -> call_contract t
    | Contract_address -> contract_address t
    | Create1 -> create1 t
    | Create2 -> create2 t
    | Delegate_call_contract -> delegate_call_contract t
    | Emit_log -> emit_log t
    | Evm_gas_left -> evm_gas_left t
    | Evm_ink_left -> evm_ink_left t
    | Pay_for_memory_grow -> pay_for_memory_grow t
    | Msg_reentrant -> msg_reentrant t
    | Msg_sender -> msg_sender t
    | Msg_value -> msg_value t
    | Native_keccak256 -> native_keccak256 t
    | Read_args -> read_args t
    | Read_return_data -> read_return_data t
    | Write_result -> write_result t
    | Return_data_size -> return_data_size t
    | Static_call_contract -> static_call_contract t
    | Tx_gas_price -> tx_gas_price t
    | Tx_ink_price -> tx_ink_price t
    | Tx_origin -> tx_origin t
    | Args_len -> args_len t
    | Console -> console fmt t
  )
