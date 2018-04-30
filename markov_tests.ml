open Token ;;
open Markov ;;

(* Test MarkovChain.empty - Markov Chain Creation *)
let () =
  let m = MarkovChain.empty () in
  assert (MarkovChain.size m = (0, 0)) ;;

(* Test add *)
let () =
  let m = MarkovChain.empty () in
  MarkovChain.add m (Word "a") (Word "b");
  assert (MarkovChain.size m = (1, 1));
  MarkovChain.add m (Word "a") (Word "c");
  assert (MarkovChain.size m = (1, 2)) ;;

print_endline "ALL TESTS PASSED."
