(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Markov Module --
 * Definitions and tools for working with Markov Chain objects.
 *)

module type MARKOVCHAIN =
  sig
    type mchain

    val empty : unit -> mchain
    val add : mchain -> string -> string -> unit
    val query : mchain -> string -> string option
    val load : string -> mchain
    val save : mchain -> string -> unit
  end

module MarkovChain : MARKOVCHAIN =
  struct

    (* Markov Chain Data Structure:
     * Each pair of keys can be used to compute a probability of traversing
     * between the two keys. The probability is computed from a occurrence
     * count in `chain` divided by the universe count in `totals`.
     * INVARIANT: All first keys in `chain` must exist in `totals`. Each value
     * in `totals` corresponds to the sum of all counts under each first-level
     * key in `chain`.
     *)
    type mchain = {
      chain : (string, (string, int) Hashtbl.t) Hashtbl.t;
      totals : (string, int) Hashtbl.t
    }

    exception BadChain of string
    
    let empty () : mchain = {chain = Hashtbl.create 0; totals = Hashtbl.create 0}
    
    let add_n (n : int) (m : mchain) (s1 : string) (s2 : string) : unit =
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

    let roll (m : mchain) (s : string) : string option =
      match Hashtbl.find_opt m.chain s with
      | Some l -> let i = Random.int (Hashtbl.find m.totals s) in
        let _, r = Hashtbl.fold (fun w c (b, s) ->
          if i >= b && i < b + c then (b + c, w)
          else (b + c, s)) l (0, "") in Some r
      | None -> None

    let save (m : mchain) (p : string) : unit =
      let f = open_out p in
      Hashtbl.iter (fun t1 c -> 
        Printf.fprintf f "%s %d\n" t1 c;
        match Hashtbl.find_opt m.chain t1 with
        | None -> close_out f; 
          raise (BadChain ("Missing chain entry for " ^ t1))
        | Some h -> Hashtbl.iter 
                      (fun t2 c -> Printf.fprintf f "%s %d\n" t2 c) h)
      m.totals;
      close_out f

    let load (p : string) : mchain =
      let m = empty () in
      let f = open_in p in
      let read = ref 0 in
      let expected = ref 0 in
      try
        let rec scan_total () : unit =
          let l = input_line f in
          let t1, n = Scanf.sscanf l "%s %d" (fun t1 n -> t1, n) in
          expected := !expected + n;
          let rec scan_entries (i : int) : unit =
            if i > 0 then let l = input_line f in
              let t2, c = Scanf.sscanf l "%s %d" (fun t2 c -> t2, c) in
              add_n c m t1 t2;
              read := !read + c;
              scan_entries (i-c)
            else if i < 0 then
              raise (BadChain ("Read counts exceed totals for token " ^ t1))
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


