module type MARKOVCHAIN =
  sig
    type mchain

    val empty : unit -> mchain
    val add : mchain -> string -> string -> unit
    val roll : mchain -> string -> string option
    val load : string -> mchain
    val save : mchain -> string -> unit
  end
