(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Learner Module --
 * Uses text streams to create a markov object
 *)

open Token ;;
open Markov ;;

type model_t = {assocs : mchain; chains : mchain} ;;

class type model_class_t =
  object
    method name : string
    method depth : int
    method train : int -> token Stream.t -> unit
    method save : string -> unit
    method load : string -> unit
    method assocs : mchain
    method chains : mchain
  end ;;

class model (name : string) (depth : int) : model_class_t =
  object
    val name = name
    val depth = depth
    val mutable model : model_t = {
      assocs = MarkovChain.empty ();
      chains = MarkovChain.empty ()
    }

    method name = name
    method depth = depth

    method train (i : int) (s : token Stream.t) : unit =
      let rec train_line (acc : token list) : token list =
        let t1 = Stream.next s in
          if t1 = End then acc
          else match Stream.peek s with
               | Some t2 -> MarkovChain.add model.chains t1 t2; train_line (t1 :: acc)
               | None -> acc in
      let train_assocs (l1 : token list) (l2 : token list) =
        List.iter (fun t1 -> List.iter (fun t2 ->
          MarkovChain.add model.assocs t1 t2) l2) l1 in
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
      done with Stream.Failure -> print_endline "Done!"

    method save (n : string) : unit =
      MarkovChain.save model.assocs (n ^ ".oma");
      MarkovChain.save model.chains (n ^ ".omc") 

    method load (n : string) : unit =
      model <- {assocs = MarkovChain.load (n ^ ".oma");
       chains = MarkovChain.load (n ^ ".omc")}

    method assocs = model.assocs
    method chains = model.chains
  end
