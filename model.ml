(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Learner Module --
 * Uses text streams to create a markov object
 *)

open Token ;;
open Markov ;;

type model_t = {chains : mchain; assocs : mchain}  ;;

class type model_class_t =
  object
    method name : string
    method train : int -> token list Stream.t -> unit
    method save : string -> unit
    method load : string -> bool
    method chains : mchain
    method assocs : mchain
    method query : token list -> int -> int -> float -> token list option
    method set_debug : bool -> unit
  end ;;

class model (name : string) : model_class_t =
  object(this)
    val name = name
    val mutable model : model_t = {
      chains = MarkovChain.empty ();
      assocs = MarkovChain.empty ();
    }
    val mutable debug = false

    method name = name

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
        match MarkovChain.query this#assocs t1 t2, MarkovChain.query this#assocs t2 t1  with
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

    method train (i : int) (s : token list Stream.t) : unit =
      let rec train_line (line : token list) : unit =
        match line with
        | t1 :: t2 :: t-> 
            MarkovChain.add model.chains t1 t2; train_line (t2 :: t)
        | _ -> () in
      let train_assocs (m : mchain) (l1 : token list) (l2 : token list) =
        List.iter (fun t1 -> List.iter (fun t2 -> MarkovChain.add m t1 t2) l2) l1
      in
      let counter = ref 0 in
      (try while !counter < i || i < 0 do
        counter := !counter + 1;
        let line = Stream.next s in
        train_line line;
        train_assocs model.assocs line line;
        if !counter mod 1000 = 0 then Printf.printf "Trained %d lines...\n%!" !counter;
      done with Stream.Failure -> print_endline "Done!");
      MarkovChain.bake this#chains;
      MarkovChain.bake this#assocs;


    method save (n : string) : unit =
      Printf.printf "Saving chains...\n%!";
      MarkovChain.save this#chains (n ^ ".omc");
      Printf.printf "Saving associations...\n%!";
      MarkovChain.save this#assocs (n ^ ".oma");

    method load (n : string) : bool =
      try
        Printf.printf "Loading chains...\n%!";
        let chains = MarkovChain.load (n ^ ".omc") in
        Printf.printf "Loading associations...\n%!";
        let assocs = MarkovChain.load (n ^ ".oma") in
        model <- {chains = chains; assocs = assocs};
        true
      with Sys_error _ -> false

    method chains = model.chains
    method assocs = model.assocs

    method query (c : token list) 
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
