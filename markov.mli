open Token ;;

type mchain = {
  chain : (token, (token, int) Hashtbl.t) Hashtbl.t;
  totals : (token, int) Hashtbl.t
} ;;

module type MARKOVCHAIN =
  sig
    exception BadChain of string

    val empty : unit -> mchain
    val add : mchain -> token -> token -> unit
    val query : mchain -> token -> token -> (int * int) option
    val enumerate : mchain -> token -> ((token * int) list * int)
    val dump : mchain -> (token * ((token * int) list * int)) list
    val roll : mchain -> token -> token option
    val size : mchain -> int * int
    val load : string -> mchain
    val save : mchain -> string -> unit
  end ;;

module MarkovChain : MARKOVCHAIN ;;
