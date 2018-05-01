(* O[B]abble - A Markov Chain Chatbot in Ocaml *)
(* Copyright (c) 2018 The OBabble Team *)

open Token ;;
open Markov ;;

let corpus = "movie_lines.txt" ;;
let model_name = "movie_lines" ;;

let model = new Model.model model_name 1 ;;

(* Initialize model *)
let () =
  print_string "Loading from saved model...";
  if not (model#load model_name) then
    (print_string "Save not found.";
     print_endline "Training new model...";
     model#train 1000000 (Parser.get_stream corpus);
     model#save model_name)
  else print_endline "Done!"

(* Print glorious banner *)
let () =
  print_endline "\n\nWelcome to...";
  let banner = open_in "obabble_art.txt" in
  (try while true do
    print_endline (input_line banner)
  done with End_of_file -> ());
  print_endline "\n" ;;


let () =
  print_endline "Begin a conversation:";
  while true do
    try
      print_string "|: ";
      let seed = read_line () in
      print_string "|> ";
      (* print_endline (seed ^ " " ^ (Generator.gen model
                                  (Parser.get_user_token_list seed))) *)
      print_endline (seed ^ " " ^ (Generator.gen model.chains
                                  (Word seed)))
    with Generator.WordNotFound _ -> print_endline "..."
  done ;;
