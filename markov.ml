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
    val query : mchain -> string -> string option
    val add : mchain -> string -> string -> unit
  end

module MarkovChain : MARKOVCHAIN =
  struct
    type mchain = {
      chain : (string, (string, int) Hashtbl.t) Hashtbl.t;
      totals : (string, int) Hashtbl.t
    }
    
    let empty () : mchain = {chain = Hashtbl.create 0; totals = Hashtbl.create 0}
    
    let add (m : mchain) (s1 : string) (s2 : string) : unit =
      match Hashtbl.find_opt m.chain s1 with
      | Some l -> (match Hashtbl.find_opt l s2 with
                   | Some c -> Hashtbl.replace l s2 (c + 1)
                   | None -> Hashtbl.add l s2 1)
      | None -> (let l = Hashtbl.create 0 in
        Hashtbl.add l s2 1;
        Hashtbl.add m.chain s1 l);
      match Hashtbl.find_opt m.totals s1 with
      | Some t -> print_endline ("BOO"^(string_of_int t));
         Hashtbl.replace m.totals s1 (t + 1)
      | None -> Hashtbl.add m.totals s1 1

    let query (m : mchain) (s : string) : string option =
      match Hashtbl.find_opt m.chain s with
      | Some l -> let i = Random.int (Hashtbl.find m.totals s) in
        print_endline (string_of_int i) ;
        print_endline (string_of_int (Hashtbl.find m.totals s));
        let _, r = Hashtbl.fold (fun w c (b, s) ->
          if i >= b && i < b + c then (b + c, w)
          else (b + c, s)) l (0, "") in Some r
      | None -> None
  end


