(* Goal is to find a generator

We're using .txt files of the input *)

open Parser ;;
open Markov ;;
open Token ;;

module type GENERATOR =
  sig 
    val roll : mchain -> token -> token option
    val gen : mchain -> token -> string
  end

module Generator : GENERATOR = 
  struct
    exception WordNotFound of token
    let roll = Markov.roll
    let rec gen (m: mchain) (word: token) : string  =
	match roll m word with
	|Some w -> (match w with 
		    |End -> ""
		    |_ -> w ^ " " ^ gen m w)
	|None -> raise (WordNotFound (word))
  end
