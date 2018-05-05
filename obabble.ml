(* O[B]abble - A Markov Chain Chatbot in OCaml *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Main Frame --
 * Set up runtime resources and operate the chatbot!
 *)

let debug = false ;;

let () = Random.self_init () ;;

let cMAXTRAIN = 10000 ;;
let cSAMPLES = 20 ;;
let cMAXLENGTH = 15 ;;
let cTHRESHOLD = 0. ;;
let cHISTORY = 20 ;;

open Token ;;
open Markov ;;

let corpus = "movie_lines.txt" ;;
let model_name = "movie_lines" ;;

let read_lines (filename : string) : string list =
  let ic = open_in filename in
  let try_read () =
    try
      Some (input_line ic)
    with
      End_of_file -> None
  in
  let rec loop (acc : string list) : string list =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [] ;;

let cSTOPWORDS = string_list_to_token_list (read_lines "stop.txt") ;;

let model = new Model.model model_name ;;

(* Initialize model *)
let () =
  Printf.printf "Loading from saved model...\n%!";
  if not (model#load model_name) then
    (Printf.printf "Save not found.\n%!";
     print_endline "Training new model...";
     model#train cMAXTRAIN (Parser.get_stream corpus);
     model#save model_name)
  else Printf.printf "Model ready!%!"

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
  model#set_debug debug;
  let history = ref [] in 
  print_endline "Begin a conversation:";
  while true do
    print_string "|: ";
    let query = string_to_token_list (read_line ()) in
    history := slice cHISTORY (query @ !history);
    print_string "|> ";
    (try match model#query !history cSAMPLES cMAXLENGTH cTHRESHOLD with
    | Some response -> history := slice cHISTORY (query @ !history);
        print_endline (token_list_to_string response)
    | None -> print_endline "..."
    with Failure _ -> print_endline "...");
    print_endline "";
  done ;;
