(* Goal is to find a generator

We're using .txt files of the input *)

open Parser ;;
open Markov ;;

module type GENERATOR =
  sig 
    val roll : mchain -> string -> string option
    val gen : mchain -> token -> token list
  end

module Generator : GENERATOR = 
  struct
    let roll = MarkovChain.roll
    let rec gen (m: mchain) (word: token) : token list  =
      match roll m word with
      |Some w -> w :: gen m w
      |None -> []
  end
