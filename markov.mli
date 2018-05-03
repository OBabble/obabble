open Token ;;

module type MARKOVCHAIN =
  sig
    exception BadChain of string

    type mchain

    val empty : unit -> mchain
    val add : mchain -> token -> token -> unit
    val bake : mchain -> unit
    val token_list : mchain -> token -> token list
    val token_totals : mchain -> token -> int
    val query : mchain -> token -> token -> (int * int) option
    val roll : int -> mchain -> token -> token option
    val size : mchain -> int * int
    val load : string -> mchain
    val save : mchain -> string -> unit
  end ;;

module MarkovChain : MARKOVCHAIN ;;

type mchain = MarkovChain.mchain ;;
