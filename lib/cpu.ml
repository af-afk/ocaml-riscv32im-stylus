
type t =
  {
  (** Registers available to the program. *)
  r: Registers.t [@default Registers.empty]

  (** Program scratchpad. *)
  ; b: Memory.t [@default Memory.empty]

  (** Ethereum memory that is available in the life of the contract execution. *)
  ; e_m: Ethereum_mem.t [@default Ethereum_mem.empty]

  (** Ethereum ETH balances that are available natively with a feature. *)
  ; e_b: Ethereum_balances.t [@default Ethereum_balances.empty]

  (** Ethereum calldata that was provided to the contract. *)
  ; e_cd: Ethereum_cd.t [@default Ethereum_cd.empty]

  (** Ethereum returndata that the contract is trying to return. *)
  ; e_rd: Ethereum_cd.t [@default Ethereum_cd.empty]

  (** The current program counter. *)
  ; pc: int32 [@default 0l]

  (** The last operation that took place (for debugging/research purposes. *)
  ; last_op: Lifted.t option [@default None]}
[@@deriving show, make, qcheck2]
