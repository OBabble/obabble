(* O[B]abble - A Markov Chain Chatbot in Ocaml *)
(* Copyright (c) 2018 The OBabble Team *)

let _debug = false ;;

let cMAXTRAIN = (int_of_float 1e5) ;;
let cSAMPLES = (int_of_float 1e2) ;;
let cMAXLENGTH = 20 ;;
let cTHRESHOLD = 1000. ;;
let cHISTORY = 10 ;;

open Token ;;
open Markov ;;

let corpus = "movie_lines.txt" ;;
let model_name = "movie_lines" ;;

let model = new Model.model model_name 1 ;;

(* Initialize model *)
let () =
  Printf.printf "Loading from saved model...\n%!";
  if not (model#load model_name) then
    (Printf.printf "Save not found.\n%!";
     print_endline "Training new model...";
     model#train cMAXTRAIN (Parser.get_stream corpus);
     model#save model_name)
  else Printf.printf "Done!%!"

(* Print glorious banner *)
let () =
  print_endline "\n\nWelcome to...";
  let banner = open_in "obabble_art.txt" in
  (try while true do
    print_endline (input_line banner)
  done with End_of_file -> ());
  print_endline "\n" ;;

(* List slice utility *)
let rec slice (n : int) (l : 'a list) : 'a list = 
  if n <= 0 then []
  else match l with
  | h :: t -> h :: slice (n-1) t
  | _ -> [] ;;

(* Run conversation loop *)
let () =
  let history = ref [] in
  print_endline "Begin a conversation:";
  while true do
    print_string "|: ";
    let query = token_list (read_line ()) in
    history := slice cHISTORY (query @ !history);
    print_string "|> ";
    (try match model#query !history cSAMPLES cMAXLENGTH cTHRESHOLD with
    | Some response -> (* history := slice cHISTORY (query @ !history); *)
        print_endline (token_list_to_string response)
    | None -> print_endline "..."
    with Failure _ -> print_endline "...");
    print_endline "";
  done ;;
