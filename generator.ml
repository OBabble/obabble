(* Goal is to find a generator

We're using .txt files of the input *)

open Parser ;;
open Markov ;;
open Token ;;
open Set ;;

module type GENERATOR =
  sig
    exception WordNotFound of token
    val gen : mchain -> token -> string
  end

module StringSet = Set.Make(String)

module Generator : GENERATOR =
  struct
    exception WordNotFound of token

    let roll = MarkovChain.roll

    let rec gen (m : mchain) (word : token) : string  =
      match roll m word with
      | Some w -> (match w with
                  |End -> ""
                  |Word s -> s ^ " " ^ gen m w)
      | None -> raise (WordNotFound (word))

    let scorer (m : mchain)
               (assoc : token list)
               (ans : token list list)
             : (token list * float) =
      let assoc_set =
        List.fold_left (fun a x -> StringSet.add x a)
                       StringSet.empty
                       (token_list_to_string_list assoc)
      in
      let score_answer (assoc : string Set)
                       (ans : token list)
                     : float =






  end
