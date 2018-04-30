(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Learner Module --
 * Uses text streams to create a markov object
 *)

open Token ;;
open Markov ;;

let train (m : mchain) (s : token Stream.t) : unit =
  let rec train_line () : unit =
    let t1 = Stream.next s in
      if t1 = End then ()
      else match Stream.peek s with
           | Some t2 -> MarkovChain.add m t1 t2; train_line ()
           | None -> () in
  let counter = ref 0 in
  try while true do
    counter := !counter + 1;
    train_line ();
    if !counter mod 1000 = 0 then Printf.printf "Trained %d lines...\n%!" !counter;
  done with Stream.Failure -> print_endline "Done!" ;;

(* TODO: Probably redundant. Either adds extra information to the mchain, 
 * or just use the implementaiton in MarkovChain
 *)
let save =  MarkovChain.save ;;
let load = MarkovChain.load ;;
