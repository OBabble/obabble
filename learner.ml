(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Learner Module --
 * Uses text streams to create a markov object
 *)

open Markov ;;

let train (m : mchain) (s : token Stream.t) : unit =
  try let t1 = Stream.next s in
    match Stream.peek s with
    | Some t2 -> MarkovChain.add m t1 t2; train m s
    | None -> ()
  with Stream.Failure -> () ;;

(* TODO: Probably redundant. Either adds extra information to the mchain, 
 * or just use the implementaiton in MarkovChain
 *)
let save =  MarkovChain.save ;;
let load = MarkovChain.load ;;
