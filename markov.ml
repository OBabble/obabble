(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Markov Module --
 * Definitions and tools for working with Markov Chain objects.
 *)

open Token ;;

let cENDREPR = "|END|" ;;

(* Markov Chain Data Structure:
 * Each pair of keys (of type token) can be used to compute a probability of 
 * traversing between the two keys. The probability is computed from a 
 * occurrence count in `chain` divided by the universe count in `totals`.
 * INVARIANT: All first keys in `chain` must exist in `totals`. Each value
 * in `totals` corresponds to the sum of all counts under each first-level
 * key in `chain`.
 *)
type mchain = {
  chain : (token, (token, int) Hashtbl.t) Hashtbl.t;
  totals : (token, int) Hashtbl.t
}

module type MARKOVCHAIN =
  sig
    exception BadChain of string

    val empty : unit -> mchain
    val add : mchain -> token -> token -> unit
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
    
    let empty () : mchain = {chain = Hashtbl.create 0; totals = Hashtbl.create 0}
    
    let add_n (n : int) (m : mchain) (s1 : token) (s2 : token) : unit =
      (match Hashtbl.find_opt m.chain s1 with
       | Some l -> (match Hashtbl.find_opt l s2 with
                    | Some c -> Hashtbl.replace l s2 (c + n)
                    | None -> Hashtbl.add l s2 n)
       | None -> (let l = Hashtbl.create 0 in
         Hashtbl.add l s2 n;
         Hashtbl.add m.chain s1 l));
      (match Hashtbl.find_opt m.totals s1 with
       | Some t -> Hashtbl.replace m.totals s1 (t + n)
       | None -> Hashtbl.add m.totals s1 n)

    let add = add_n 1

    let token_list (m : mchain) (t : token) : token list =
      Hashtbl.fold (fun t _ acc -> t :: acc) (Hashtbl.find m.chain t) []

    let token_totals (m : mchain) (t : token) : int =
      Hashtbl.find m.totals t

    let query (m : mchain) (t1 : token) (t2 : token) : (int * int) option =
      try
        Some (Hashtbl.find (Hashtbl.find m.chain t1) t2,
          token_totals m t1)
      with Not_found -> None

    let roll (m : mchain) (s : token) : token option =
      match Hashtbl.find_opt m.chain s with
      | Some l -> let i = Random.int (token_totals m s) in
        let _, r = Hashtbl.fold (fun w c (b, s) ->
          (* This is guaranteed to return some result from the chain. *)
          if i >= b && i < b + c then (b + c, w)
          else (b + c, s)) l (0, End) in Some r
      | None -> None
    
    let size (m : mchain) : int * int =
      (Hashtbl.length m.totals,
        Hashtbl.fold (fun t _ acc -> acc + token_totals m t) m.totals 0)

    let save (m : mchain) (p : string) : unit =
      let f = open_out p in
      let write_token (t : token) 
                      (n : int) 
                      (l : int)
                      (cb : string -> unit) : unit =
        if l = 1 then Printf.fprintf f "--|"; (* Print tab for formatting *)
        match t with
        | End -> Printf.fprintf f "%s %d\n" cENDREPR n
        | Word s -> Printf.fprintf f "%s %d\n" s n; cb s in
      Hashtbl.iter (fun t1 c -> 
        write_token t1 c 0 (fun s ->
          match Hashtbl.find_opt m.chain t1 with
          | None -> close_out f; 
            raise (BadChain ("Missing chain entry for " ^ s))
          | Some h -> Hashtbl.iter 
            (fun t2 c -> write_token t2 c 1 (fun _ -> ()))  h)) m.totals;
      close_out f

    (* TODO: CLEAN UP THIS CODE! *)
    let load (p : string) : mchain =
      let m = empty () in
      let f = open_in p in
      let line = ref 0 in
      let read = ref 0 in
      let expected = ref 0 in
      let parse_line (s : string) (l : int) : token * int =
        let process = (fun s n ->
            if s = cENDREPR then End, n
            else Word s, n) in
        try
          (* TODO: Yuck *)
          if l = 1 then Scanf.sscanf s "--|%s %d" process
          else Scanf.sscanf s "%s %d" process
        with
        | Scanf.Scan_failure e -> print_endline s; Printf.printf ">>%d %d\n%!" l
        !line;
            failwith e

        | End_of_file ->          raise (BadChain 
            (Printf.sprintf "Improper format at line %d" !line))
      in
      try
        let rec scan_total () : unit =
          let s = input_line f in
          line := !line + 1;
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
                       | End -> cENDREPR) in
              raise (BadChain 
                (Printf.sprintf 
                  "Read more than expected for token %s at line %d" e !line))
            in
          scan_entries n;
          scan_total () in
        scan_total (); m 
        (* TODO: This structure is weird; this m will never be
         * reached. Rewrite to test if file is EOF rather than using neat pythonic
         * exception-handling behavior *)
      with End_of_file -> if !read <> !expected then
        raise (BadChain "EOF while expecting more chain data")
      else m
  end


