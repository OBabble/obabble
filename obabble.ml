(* O[B]abble - A Markov Chain Chatbot in Ocaml *)
(* Copyright (c) 2018 The OBabble Team *)

open Token ;;
open Parser ;;
open Learner ;;
open Markov ;;

let corpus = "movie_lines.txt" ;;
let model_filename = "movie_lines.mc" ;;

let model =
  if Sys.file_exists model_filename then
    MarkovChain.load model_filename
  else let model = MarkovChain.empty () in
  train model (Parser.get_stream corpus);
  MarkovChain.save model model_filename; model;;
