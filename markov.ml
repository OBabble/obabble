(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Markov Module --
 * Definitions and tools for working with Markov Chain objects.
 *)

type token = 
  | Word of string
  | End ;;

let cENDREPR = "|END|" ;;

module type MARKOVCHAIN =
  sig
    type mchain

    val empty : unit -> mchain
    val add : mchain -> token -> token -> unit
    val roll : mchain -> token -> token option
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
      chain : (token, (token, int) Hashtbl.t) Hashtbl.t;
      totals : (token, int) Hashtbl.t
    }

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

    let roll (m : mchain) (s : token) : token option =
      match Hashtbl.find_opt m.chain s with
      | Some l -> let i = Random.int (Hashtbl.find m.totals s) in
        let _, r = Hashtbl.fold (fun w c (b, s) ->
          (* This is guaranteed to return some result from the chain. *)
          if i >= b && i < b + c then (b + c, w)
          else (b + c, s)) l (0, End) in Some r
      | None -> None

    let save (m : mchain) (p : string) : unit =
      let f = open_out p in
      let write_token (t : token) (n : int) (cb : string -> unit): unit =
        match t with
        | End -> Printf.fprintf f "%s %d\n" cENDREPR n
        | Word s -> Printf.fprintf f "%s %d\n" s n; cb s in
      Hashtbl.iter (fun t1 c -> 
        write_token t1 c (fun s ->
          match Hashtbl.find_opt m.chain t1 with
          | None -> close_out f; 
            raise (BadChain ("Missing chain entry for " ^ s))
          | Some h -> Hashtbl.iter 
            (fun t2 c -> write_token t2 c (fun _ -> ()))  h)) m.totals;
      close_out f

    let load (p : string) : mchain =
      let m = empty () in
      let f = open_in p in
      let read = ref 0 in
      let expected = ref 0 in
      let parse_line (s : string) (n : int) : token * int =
        if s = cENDREPR then End, n
        else Word s, n in
      try
        let rec scan_total () : unit =
          let l = input_line f in
          let t1, n = Scanf.sscanf l "%s %d" parse_line in
          expected := !expected + n;
          let rec scan_entries (i : int) : unit =
            if i > 0 then let l = input_line f in
              let t2, c = Scanf.sscanf l "%s %d\n" parse_line in
              add_n c m t1 t2;
              read := !read + c;
              scan_entries (i-c)
            else if i < 0 then
              let e = (match t1 with 
                       | Word s -> s
                       | End -> cENDREPR) in
              raise (BadChain ("Read more than expected for token " ^ e))
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


