(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Learner Module --
 * Uses text streams to create a markov object
 *)

open Token ;;
open Markov ;;

type model_t = {chains : mchain; assocs : mchain; iassocs : mchain}  ;;

class type model_class_t =
  object
    method name : string
    method depth : int
    method train : int -> token Stream.t -> unit
    method save : string -> unit
    method load : string -> bool
    method chains : mchain
    method assocs : mchain
    method iassocs : mchain
    method query : token list -> token list -> int -> int -> 
      float -> token list option
    method set_debug : bool -> unit
  end ;;

class model (name : string) (depth : int) : model_class_t =
  object(this)
    val name = name
    val depth = depth
    val mutable model : model_t = {
      chains = MarkovChain.empty ();
      assocs = MarkovChain.empty ();
      iassocs = MarkovChain.empty ()
    }
    val mutable debug = false

    method name = name
    method depth = depth

    (* Unexposed util *)  
    method private gen (len : int) (m: mchain) (word: token) : token list  =
      if len <= 0 then [End]
      else match MarkovChain.roll m word with
      | Some w -> (match w with
            | End -> [End]
            | Start
            | Word _ -> w :: (this#gen (len-1) m w))
      | None -> []

    (* Unexposed util *)  
    method private score (q : token list) (ans : token list list) : (token list * float) list =
      let pair_relevance (t1 : token) (t2 : token) : float =
        match MarkovChain.query this#iassocs t1 t2, MarkovChain.query this#iassocs t2 t1  with
        | Some (n1, t1), Some (n2, t2) ->
            (* Sort of like TF-IDF *)
            ((float n1) /. log (float t1) /. log (float (t2 - n2 + 1)))
        | _ -> 0. in 
      let score_answer (ans : token list) : (token list * float) =
        if Random.float 1. < 0.0 then (ans, 0.) (* Random dropout *) 
        else (if debug then (print_endline ""; print_endline (token_list_to_string ans));
        let _, raw_coherency = List.fold_left (fun (t1, acc) t2 ->
          match MarkovChain.query this#chains t1 t2 with
          | Some (n, t) -> (t2, acc +. ((float n) /. (log (float t) +. 1.)))
          | None -> (t2, acc)) (List.hd ans, 0.) ans in
        let coherency = raw_coherency /. (float (List.length ans)) in
        let raw_relevance = List.fold_left (fun a q_elt -> a +.
                      (List.fold_left (fun acc a_elt ->
                        acc +. pair_relevance q_elt a_elt) 0. ans)) 0. q in
        let relevance = raw_relevance /. log (float ((List.length q) + 2)) in
        if debug then 
          Printf.printf "COHERENCY: %f RELEVANCE: %f\n" coherency relevance;
        let score = coherency *. relevance in
        if debug then Printf.printf "FINAL SCORE: %f\n" score; (ans, score)) in
      List.map score_answer ans 

    (* Unexposed util *)  
    method private stop_filter (t : token list) (stop : token list) : token list =
      List.filter (fun x -> not (List.mem x stop)) t

    (*method profanity_filter *)

    method train (i : int) (s : token Stream.t) : unit =
      let rec train_line (acc : token list) : token list =
        let t1 = Stream.next s in
          if t1 = End then acc
          else match Stream.peek s with
               | Some t2 -> MarkovChain.add model.chains t1 t2; train_line (t1 :: acc)
               | None -> acc in
      let train_assocs (m : mchain) (l1 : token list) (l2 : token list) =
        List.iter (fun t1 -> List.iter (fun t2 ->
          MarkovChain.add m t1 t2) l2) l1 in
      let counter = ref 0 in
      let prev_line = ref None in
      (try while !counter < i || i < 0 do
        counter := !counter + 1;
        let line = train_line [] in
        train_assocs model.iassocs line line;
        (match !prev_line with
        | Some pline -> train_assocs model.assocs pline line
        | None -> ());
        prev_line := Some line;
        if !counter mod 1000 = 0 then Printf.printf "Trained %d lines...\n%!" !counter;
      done with Stream.Failure -> print_endline "Done!");
      MarkovChain.bake this#chains;
      MarkovChain.bake this#assocs;
      MarkovChain.bake this#iassocs


    method save (n : string) : unit =
      Printf.printf "Saving chains...\n%!";
      MarkovChain.save this#chains (n ^ ".omc");
      Printf.printf "Saving associations...\n%!";
      MarkovChain.save this#assocs (n ^ ".oma");
      Printf.printf "Saving intra-sentence associations...\n%!";
      MarkovChain.save this#iassocs (n ^ ".omi");

    method load (n : string) : bool =
      try
        Printf.printf "Loading chains...\n%!";
        let chains = MarkovChain.load (n ^ ".omc") in
        Printf.printf "Loading associations...\n%!";
        let assocs = MarkovChain.load (n ^ ".oma") in
        Printf.printf "Loading intra-sentence associations...\n%!";
        let iassocs = MarkovChain.load (n ^ ".omi") in
        model <- {chains = chains; assocs = assocs; iassocs = iassocs};
        true
      with Sys_error _ -> false

    method chains = model.chains
    method assocs = model.assocs
    method iassocs = model.iassocs

    method query (s : token list) (c : token list) 
    (n : int) (m : int) (t : float) : token list option =
      let rec repeat (n : int) (t : token) : token list =
        if n > 0 then t :: repeat (n-1) t
        else [] in
      let seed_pool = repeat n Start in
      let candidates = List.map (fun s ->
        s :: (this#gen m this#chains s)) seed_pool in
      let scored = this#score c candidates in
      let best, score = List.hd 
        (List.sort (fun (_, a) (_, b) -> compare b a) scored) in
      if debug then (print_endline "\n\n\n";
      Printf.printf "(score: %f)\n" score);
      if score > t then Some best
      else None

    method set_debug (t : bool) : unit = debug <- t
  end
