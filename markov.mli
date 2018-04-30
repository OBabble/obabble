module type MARKOVCHAIN =
  sig
    type mchain

    val empty : unit -> mchain
    val add : mchain -> token -> token -> unit
    val roll : mchain -> token -> token option
    val load : string -> mchain
    val save : mchain -> string -> unit
  end
