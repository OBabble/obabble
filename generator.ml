(* Goal is to find a generator

We're using .txt files of the input *)

open Parser ;;
open Markov ;;
open Token ;;

exception WordNotFound of token

let roll = MarkovChain.roll

let rec gen (m: mchain) (word: token) : token list  =
  match roll m word with
  | Some w -> (match w with
        | End -> []
        | Word _ -> w :: (gen m w))
  | None -> raise (WordNotFound (word))

let scorer (m : mchain)
           (q : token list)
           (ans : token list list)
         : (token list * float) =
  let compare_score (_, f1 : token list * float)
                    (_, f2 : token list * float)
                  : int =
    compare f1 f2
  in
  let weighted_subscore (w : (int * int) option) : float =
    match w with
    | None -> 0.
    | Some (n, t) -> (float n) /. (float t)
  in
  let score_answer (m : mchain)
                   (q : token list)
                   (ans : token list)
                 : (token list * float) =
    let subscore = List.fold_left (fun a q_elt -> a +.
                  (List.fold_left (fun acc a_elt -> acc +. weighted_subscore
                  (MarkovChain.query m q_elt a_elt)) 0. ans)) 0. q
    in
    (ans, subscore /. (float (List.length ans)))
  in
  let score_list = List.sort compare_score (List.map (score_answer m q) ans) in
  List.hd score_list ;;


let rec repeat (m: mchain) (word: token) (n : int) : token list list =
  if n > 0 then (gen m word) :: (repeat m word (n-1))
  else []
