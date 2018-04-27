module type MARKOVCHAIN =
  sig
    type mchain

    val empty : unit -> mchain
    val query : mchain -> string -> string option
    val add : mchain -> string -> string -> unit
  end
