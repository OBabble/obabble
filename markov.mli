module type MARKOVCHAIN =
  sig
    type mchain

    val empty : unit -> mchain
    val add : mchain -> string -> string -> unit
    val query : mchain -> string -> string option
  end
