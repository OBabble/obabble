(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Learner Module --
 * Uses text streams to create a markov object
 *)

open Token ;;
open Markov ;;

type model = {assocs : mchain; chains : mchain} ;;

let empty () : model = {
  assocs = MarkovChain.empty (); 
  chains = MarkovChain.empty () 
} ;;

let train (m : model) (d : int) (i : int) (s : token Stream.t) : unit =
  let rec train_line (acc : token list) : token list =
    let t1 = Stream.next s in
      if t1 = End then acc
      else match Stream.peek s with
           | Some t2 -> MarkovChain.add m.chains t1 t2; train_line (t1 :: acc)
           | None -> acc in
  let rec train_assocs (l1 : token list) (l2 : token list) =
    List.iter (fun t1 -> List.iter (fun t2 ->
      MarkovChain.add m.assocs t1 t2) l2) l1 in
  let counter = ref 0 in
  let prev_line = ref None in
  try while !counter < i || i < 0 do
    counter := !counter + 1;
    let line = train_line [] in
    (match !prev_line with
    | Some pline -> train_assocs pline line
    | None -> ());
    prev_line := Some line;
    if !counter mod 1000 = 0 then Printf.printf "Trained %d lines...\n%!" !counter;
  done with Stream.Failure -> print_endline "Done!" ;;

(* TODO: Probably redundant. Either adds extra information to the mchain, 
 * or just use the implementaiton in MarkovChain
 *)
let save (m : model) (n : string) : unit =
  MarkovChain.save m.assocs (n ^ ".oma");
  MarkovChain.save m.chains (n ^ ".omc") ;;
let load (n : string) : model =
  {assocs = MarkovChain.load (n ^ ".oma");
   chains = MarkovChain.load (n ^ ".omc")} ;;
