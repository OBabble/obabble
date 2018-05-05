(* O[B]abble - A Markov Chain Chatbot in OCaml *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Markov Module --
 * Definitions and tools for working with Markov Chain objects.
 *)

open Token ;;

let cSTARTREPR = "|START|" ;;
let cENDREPR = "|END|" ;;


module type MARKOVCHAIN =
  sig
    exception BadChain of string

    type mchain

    val empty : unit -> mchain
    val add : mchain -> token -> token -> unit
    val bake : mchain -> unit
    val token_list : mchain -> token -> token list
    val token_totals : mchain -> token -> int
    val query : mchain -> token -> token -> (int * int) option
    val roll : mchain -> token -> token option
    val size : mchain -> int * int
    val load : string -> mchain
    val save : mchain -> string -> unit
  end ;;

module MarkovChain : MARKOVCHAIN =
  struct    
    exception BadChain of string
    exception RollWithoutBake

    type token_tree = 
      | Node of ((token * int * int) * token_tree * token_tree) 
      | Leaf

    let rec tree_find (t : token_tree) (i : int) : token =
      match t with 
      | Node ((t, s, e), l, r) -> if i >= s && i < e then t
                                  else if i < s then tree_find l i 
                                  else tree_find r i
      | Leaf -> raise Not_found 

    let rec tree_build (a : (token * int * int) array) : token_tree = 
      match a with
      | [| |] -> Leaf
      | [| l; r |] -> Node (l, Leaf, Node (r, Leaf, Leaf))
      | _ -> let k = (Array.length a) / 2 in
             Node (a.(k), 
              tree_build (Array.sub a 0 k), 
              tree_build (Array.sub a (k+1) (Array.length a - (k+1))))

    (* Markov Chain Data Structure:
     * Each pair of keys (of type token) can be used to compute a probability of 
     * traversing between the two keys. The probability is computed from a 
     * occurrence count in `chain` divided by the universe count in `totals`.
     * INVARIANT: All first keys in `chain` must exist in `totals`. Each value
     * in `totals` corresponds to the sum of all counts under each first-level
     * key in `chain`.
     *)

    type mchain = {
      mutable baked : bool;
      chain : (token, (token_tree * (token, int) Hashtbl.t)) Hashtbl.t;
      totals : (token, int) Hashtbl.t
    }
    
    let empty () : mchain = {
      baked = false;
      chain = Hashtbl.create 0; 
      totals = Hashtbl.create 0
    }
    
    let add_n (n : int) (m : mchain) (s1 : token) (s2 : token) : unit =
      if n > 0 then m.baked <- false;
      (match Hashtbl.find_opt m.chain s1 with
       | Some (_, l) -> (match Hashtbl.find_opt l s2 with
                    | Some c -> Hashtbl.replace l s2 (c + n)
                    | None -> Hashtbl.add l s2 n)
       | None -> (let l = Hashtbl.create 0 in
         Hashtbl.add l s2 n;
         Hashtbl.add m.chain s1 (Leaf, l)));
      (match Hashtbl.find_opt m.totals s1 with
       | Some t -> Hashtbl.replace m.totals s1 (t + n)
       | None -> Hashtbl.add m.totals s1 n)

    let add = add_n 1
    
    let augmented_token_list (m : mchain) (t : token) : (token * int * int) list =
      let _, h = Hashtbl.find m.chain t in
      let _, l = Hashtbl.fold 
        (fun t c (i, l) -> (i + c, (t, i, i+c) :: l)) h (0, []) in List.rev l

    let token_list (m : mchain) (t : token) : token list =
      List.map (fun (t, _, _) -> t) (augmented_token_list m t)

    let token_totals (m : mchain) (t : token) : int =
      Hashtbl.find m.totals t

    (* Bake computes token index arrays for efficient roll. Must be called
     * before performing rolls otherwise roll will always fail. *)
    let bake (m : mchain) : unit = 
      if not m.baked then (Printf.printf "Baking...%!"; 
        Hashtbl.iter (fun t (_, h) ->
          Hashtbl.replace m.chain t 
            (tree_build (Array.of_list (augmented_token_list m t)), h)) m.chain;
        m.baked <- true;
        Printf.printf "Done!\n%!")
        
    let query (m : mchain) (t1 : token) (t2 : token) : (int * int) option =
      try let _, h = Hashtbl.find m.chain t1 in
        Some (Hashtbl.find h t2, token_totals m t1)
      with Not_found -> None

    (* Try to pick a word randomly for `n` attempts. If no word is chosen by
     * then, then just pick a random word.
     *)
    let roll (m : mchain) (s : token) : token option =
      if not m.baked then raise RollWithoutBake
      else try let tt, _ = Hashtbl.find m.chain s in         
        let i = Random.int (token_totals m s) in
        Some (tree_find tt i)
      with Not_found -> None
    
    let size (m : mchain) : int * int =
      (Hashtbl.length m.totals,
        Hashtbl.fold (fun t _ acc -> acc + token_totals m t) m.totals 0)

    let save (m : mchain) (p : string) : unit =
      let f = open_out p in
      let c_ex, t_ex = size m in Printf.fprintf f "%d %d\n" c_ex t_ex;
      let write_token (t : token) 
                      (n : int) 
                      (l : int)
                      (cb : string -> unit) : unit =
        if l = 1 then Printf.fprintf f "\t"; (* Print tab for formatting *)
        let s = match t with
        | End -> cENDREPR
        | Start -> cSTARTREPR
        | Word s -> s in
        Printf.fprintf f "%s %d\n" s n; cb s in
      Hashtbl.iter (fun t1 c -> 
        write_token t1 c 0 (fun s ->
          match Hashtbl.find_opt m.chain t1 with
          | None -> close_out f; 
            raise (BadChain ("Missing chain entry for " ^ s))
          | Some (_, h) -> Hashtbl.iter 
            (fun t2 c -> write_token t2 c 1 (fun _ -> ()))  h)) m.totals;
      Printf.printf "Saved %d tokens totaling %d instances.\n" c_ex t_ex;
      close_out f

    (* TODO: CLEAN UP THIS CODE! *)
    let load (p : string) : mchain =
      let m = empty () in
      let f = open_in p in
      let c_ex, _ = Scanf.sscanf (input_line f) "%d %d" (fun a b -> a, b) in
      let line = ref 0 in
      let loaded = ref 0 in
      let read = ref 0 in
      let expected = ref 0 in
      let parse_line (s : string) (l : int) : token * int =
        let process = (fun s n ->
            if s = cENDREPR then End, n
            else if s = cSTARTREPR then Start, n
            else Word s, n) in
        try
          (* TODO: Yuck *)
          if l = 1 then Scanf.sscanf s "\t%s %d" process
          else Scanf.sscanf s "%s %d" process
        with
        | Scanf.Scan_failure _
        | End_of_file ->          raise (BadChain 
            (Printf.sprintf "Improper format at line %d" !line))
      in
      try
        let rec scan_total () : unit =
          let s = input_line f in
          line := !line + 1;
          loaded := !loaded + 1;
          let t1, n = parse_line s 0 in
          expected := !expected + n;
          let rec scan_entries (i : int) : unit =
            if i > 0 then (let s = input_line f in
              line := !line + 1;
                let t2, c = parse_line s 1 in
                add_n c m t1 t2;
                read := !read + c;
                scan_entries (i-c)
                )
            else if i < 0 then
              let e = (match t1 with 
                       | Word s -> s
                       | Start -> cSTARTREPR
                       | End -> cENDREPR) in
              raise (BadChain 
                (Printf.sprintf 
                  "Read more than expected for token %s at line %d" e !line))
            in
          scan_entries n;
          if !loaded mod 100 = 0 then 
            Printf.printf "\rLoaded %d tokens (%.2f%%)   %!" !loaded
              ((float !loaded) /. (float c_ex) *. 100.);
          scan_total () in
        scan_total (); m 
        (* TODO: This structure is weird; this m will never be
         * reached. Rewrite to test if file is EOF rather than using neat pythonic
         * exception-handling behavior *)
      with End_of_file -> if !read <> !expected then
        raise (BadChain "EOF while expecting more chain data")
      else (Printf.printf "\rLoaded %d tokens (100.00%%)\n%!" !loaded; bake m; m)
  end ;;

type mchain = MarkovChain.mchain ;;


