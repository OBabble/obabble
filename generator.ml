(* Goal is to find a generator

We're using .txt files of the input *)

open Parser ;;
open Markov ;;

module type GENERATOR =
  sig 
    val roll : mchain -> string -> string option
    val gen : mchain -> token -> string
  end

module Generator : GENERATOR = 
  struct
    let roll = Markovchain.roll
    let rec gen (m: mchain) (word: token) : string  =
      match roll m word with
      |Some w -> match w with 
      |End -> ""
      |_ -> w ^ " " ^ gen m w
      |None -> []
  end
