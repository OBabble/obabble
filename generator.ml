(* Goal is to find a generator

We're using .txt files of the input *)

open Parser ;;
open Markov ;;
open Token ;;

module type GENERATOR =
  sig
    exception WordNotFound of token
    val gen : mchain -> token -> string
  end

module Generator : GENERATOR =
  struct
    exception WordNotFound of token
    let roll = MarkovChain.roll
    let rec gen (m: mchain) (word: token) : string  =
      match roll m word with
      |Some w -> (match w with
            |End -> ""
            |Word s -> s ^ " " ^ gen m w)
      |None -> raise (WordNotFound (word))
    let scorer (m : mchain) (query : token list) (answer : token list) : float =
      let string_list = List.map token_to_string l in


  end
