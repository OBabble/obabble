(* Goal is to find a generator

We're using .txt files of the input *)

open Parser ;;
open Markov ;;
open Token ;;
open Set ;;

exception WordNotFound of token

let roll = MarkovChain.roll

let rec gen (m: mchain) (word: token) : token list  =
  match roll m word with
  |Some w -> (match w with
        | End -> []
        | Word s -> s :: gen m w)
  |None -> raise (WordNotFound (word))

let scorer (m : mchain) (query : token list) (answer : token list) : float =
  let string_list = List.map token_to_string l in

let rec repeat (m: mchain) (word: token) : token list list =
  let reps = reps + 1 in
      if reps <= 10 then gen m word :: repeat m word
else gen m word :: []
