open Token ;;
open Markov ;;

let () =
  (* Test empty *)
  let m = MarkovChain.empty () in
  assert (MarkovChain.size m = (0, 0));

  (* Test add and size *)
  MarkovChain.add m (Word "a") (Word "b");
  assert (MarkovChain.size m = (1, 1));
  MarkovChain.add m (Word "a") (Word "c");
  assert (MarkovChain.size m = (1, 2));
  MarkovChain.add m (Word "b") (Word "c");
  assert (MarkovChain.size m = (2, 3));
  
  (* Test query *)
  assert (MarkovChain.query m (Word "a") (Word "b") = Some (1, 2));
  assert (MarkovChain.query m (Word "a") (Word "c") = Some (1, 2));
  assert (MarkovChain.query m (Word "b") (Word "c") = Some (1, 1));
  assert (MarkovChain.query m (Word "b") (Word "d") = None);

  (* Test roll *)
  (for _ = 1 to 100 do
    assert (MarkovChain.roll m (Word "a") <> None);
    assert (MarkovChain.roll m (Word "b") = Some (Word "c"));
    assert (MarkovChain.roll m (Word "c") = None);
  done);

  (* Test save and load *)
  if Sys.file_exists "test.mc" then Sys.remove "test.mc";
  assert (not (Sys.file_exists "test.mc"));
  MarkovChain.save m "test.mc";
  assert (Sys.file_exists "test.mc");
  let l = MarkovChain.load "test.mc" in
  assert (MarkovChain.query l (Word "a") (Word "b") = Some (1, 2));
  assert (MarkovChain.query l (Word "a") (Word "c") = Some (1, 2));
  assert (MarkovChain.query l (Word "b") (Word "c") = Some (1, 1));
  assert (MarkovChain.query l (Word "b") (Word "d") = None);

print_endline "ALL TESTS PASSED."
