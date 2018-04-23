(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Markov Module --
 * Definitions and tools for working with Markov Chain objects.
 *)

module MarkovChain
  struct
    type mchain = {
      chain : (string, (string, int) Hashtbl.t) Hashtbl.t;
      totals : (string, int) Hashtbl.t
    }
    
    let empty () : mchain = {chain = Hashtbl.create 0; totals = Hashtbl.create 0}
    let query (m : mchain) (s : string) : string =


  end


