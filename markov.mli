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
    val token_list : mchain -> token -> token list
    val token_totals : mchain -> token -> int
    val query : mchain -> token -> token -> (int * int) option
    val roll : mchain -> token -> token option
    val size : mchain -> int * int
    val load : string -> mchain
    val save : mchain -> string -> unit
  end ;;

module MarkovChain : MARKOVCHAIN ;;
