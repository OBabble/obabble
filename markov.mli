open Parser ;;

type mchain = {
  chain : (token, (token, int) Hashtbl.t) Hashtbl.t;
  totals : (token, int) Hashtbl.t
}

module type MARKOVCHAIN =
  sig
    exception BadChain of string

    val empty : unit -> mchain
    val add : mchain -> token -> token -> unit
    val roll : mchain -> token -> token option
    val load : string -> mchain
    val save : mchain -> string -> unit
  end ;;

module MarkovChain : MARKOVCHAIN ;;
