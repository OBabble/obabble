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
    method query : token list -> int -> int -> float -> token list option
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

    method name = name
    method depth = depth

    (* Unexposed util *)  
    method private gen (len : int) (m: mchain) (word: token) : token list  =
      if len <= 0 then []
      else match MarkovChain.roll m word with
      | Some w -> (match w with
            | End -> []
            | Start
            | Word _ -> w :: (this#gen (len-1) m w))
      | None -> []

    (* Unexposed util *)  
    method private score (q : token list) (ans : token list list) : (token list * float) list =
      let weighted_subscore (t1 : token) (t2 : token) : float =
        let rev = try MarkovChain.token_totals this#assocs t2 with Not_found -> 1 in
        match MarkovChain.query this#assocs t1 t2, MarkovChain.query this#iassocs t2 t1  with
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

    method query (s : token list) (n : int) (m : int) (t : float) : token list option =
      let rec repeat (n : int) (t : token) : token list =
        if n > 0 then t :: repeat (n-1) t
        else [] in
      let seed_pool = repeat n Start in
      let candidates = List.map (fun s ->
        s :: (this#gen m this#chains s)) seed_pool in
      let scored = this#score s candidates in
      (* List.iter (fun (l, s) ->
        print_endline "";
        print_endline (token_list_to_string l);
        Printf.printf "-->(score %f)\n" s) scored; *)
      let best, score = List.hd 
        (List.sort (fun (_, a) (_, b) -> compare b a) scored) in
      (* print_endline "\n\n\n";
      Printf.printf "(score: %f)\n" score; *)
      (* print_endline (token_list_to_string best); *)
      if score > t then Some best
      else None
  end
