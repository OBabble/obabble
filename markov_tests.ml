open Markov ;;

(* Test MarkovChain.empty - Markov Chain Creation *)
let () =
  let m = MarkovChain.empty () in
  assert (Hashtbl.length m.chain == 0);
  assert (Hashtbl.length m.total == 0) ;;
