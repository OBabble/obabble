(* Goal is to find a generator

We're using .txt files of the input *)

open Markov ;;
open Token ;;

exception WordNotFound of token

let roll = MarkovChain.roll ;;

let rec gen (t : int) (m: mchain) (word: token) : token list  =
  if t <= 0 then []
  else match roll m word with
  | Some w -> (match w with
        | End -> []
        | Word _ -> w :: (gen (t-1) m w))
  | None -> [] ;;

let score (m : mchain) (q : token list) (ans : token list list)
  : (token list * float) list =
  let weighted_subscore (t1 : token) (t2 : token) : float =
    let t2_freq = try MarkovChain.token_totals m t2 with Not_found -> 1 in
    match MarkovChain.query m t1 t2 with
    | None -> 0.
    | Some (n, t) -> (float n) /. (float t) /. (float t2_freq) in

  let score_answer (q : token list) (ans : token list) : (token list * float) =
    let subscore = List.fold_left (fun a q_elt -> a +.
                  (List.fold_left (fun acc a_elt ->
                    acc +. weighted_subscore q_elt a_elt) 0. ans)) 0. q in
    (ans, subscore /. (float (List.length ans))) in

  List.map (score_answer q) ans ;;

let stop_filter (t : token list) (stop : token list) : token list =
  List.filter (fun x -> not List.mem x stop) t ;;

let rec roll_n (n : int) (m : mchain) (t : token) : token list =
  if n > 0 then match MarkovChain.roll m t with
  | Some e -> e :: (roll_n (n-1) m t)
  | None -> roll_n (n-1) m t
  else [] ;;
