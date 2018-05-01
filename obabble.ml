(* O[B]abble - A Markov Chain Chatbot in Ocaml *)
(* Copyright (c) 2018 The OBabble Team *)

open Token ;;
open Parser ;;
open Markov ;;
open Generator ;;

let corpus = "movie_lines.txt" ;;
let model_filename = "movie_lines.mc" ;;

let model =
  if Sys.file_exists model_filename then
    (print_endline"Loading from file...";
    MarkovChain.load model_filename)
  else let model = Learner.empty () in
    print_endline "Training new model...";
    train model (Parser.get_stream corpus);
    MarkovChain.save model model_filename; model;;

(* Print glorious banner *)
let () =
  let banner = open_in "obabble_art.txt" in
  try while true do
    print_endline (input_line banner)
  done with End_of_file -> () ;;

let () =
  while true do
    try
      print_string "|: ";
      let seed = read_line () in
      print_string "|> ";
      (* print_endline (seed ^ " " ^ (Generator.gen model
                                  (Parser.get_user_token_list seed))) *)
      print_endline (seed ^ " " ^ (Generator.gen model
                                  (Word seed)))
    with Generator.WordNotFound _ -> print_endline "..."
  done ;;

