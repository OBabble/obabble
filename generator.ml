(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * Generator --
 * Definitions and tools for working with Markov Chains to generate output.
 *)

open Markov ;;
open Token ;;

let rec gen (t : int) (m: mchain) (word: token) : token list  =
  if t <= 0 then []
  else match MarkovChain.roll m word with
  | Some w -> (match w with
        | End -> []
        | Start
        | Word _ -> w :: (gen (t-1) m w))
  | None -> [] ;;

let score (assocs : mchain) (iassocs : mchain) (q : token list) (ans : token list list)
  : (token list * float) list =
  let weighted_subscore (t1 : token) (t2 : token) : float =
    let rev = try MarkovChain.token_totals assocs t2 with Not_found -> 1 in
    match MarkovChain.query assocs t1 t2, MarkovChain.query iassocs t2 t1  with
    | Some (n1, t1), Some (n2, t2) ->
        (float n1) /. (log (float t1) +. log (float rev)) +.
        (float n2) /. log (float t2)
    | _ -> 0. in
    (* This is sort of TF-IDF *)

  let score_answer (ans : token list) : (token list * float) =
    let subscore = List.fold_left (fun a q_elt -> a +.
                  (List.fold_left (fun acc a_elt ->
                    acc +. weighted_subscore q_elt a_elt) 0. ans)) 0. q in
    (ans, subscore /. sqrt (float ((List.length ans) + 1))) in
  List.map score_answer ans ;;

let stop_filter (t : token list) (stop : token list) : token list =
  List.filter (fun x -> not (List.mem x stop)) t ;;
