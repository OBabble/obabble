(* O[B]abble Chatroom - A Bot Chatroom for O[B]abble *)
(* Copyright (c) 2018 The OBabble Team *)

let debug = false ;;

let () = Random.self_init () ;;

let cMAXTRAIN = 100000 ;;
let cSAMPLES = 20 ;;
let cMAXLENGTH = 15 ;;
let cTHRESHOLD = 0. ;;
let cHISTORY = 20 ;;

open Token ;;
open Markov ;;

(* List slice utility *)
let rec slice (n : int) (l : 'a list) : 'a list =
  if n <= 0 then []
  else match l with
  | h :: t -> h :: slice (n-1) t
  | _ -> [] ;;

type bot_spec = {name : string; corpus : string} ;;

let bot_specs : bot_spec list = [
  {name = "shieber"; corpus = "shieber.txt"};
  {name = "terminator"; corpus = "terminator.txt"};
] ;;

let model_from_spec (s : bot_spec) : Model.model =
  let model = new Model.model s.name in
  Printf.printf "Preparing bot \"%s\"...\n%!" s.name;
  Printf.printf "Loading from saved model...\n%!";
  (if not (model#load s.name) then
    (Printf.printf "Save not found.\n%!";
     print_endline "Training new model...";
     model#train cMAXTRAIN (Parser.get_stream s.corpus);
     model#save s.name)
  else Printf.printf "Model ready!\n\n%!");
  model#set_debug debug;
  model ;;

let bots = List.map model_from_spec bot_specs ;;

let query_bot 
  (q : token list) (h : token list ref) (model : Model.model) : unit = 
    Printf.printf "|%s|> " model#name;
    (try match model#query q !h cSAMPLES cMAXLENGTH cTHRESHOLD with
    | Some response -> h := slice cHISTORY (q @ !h);
        print_endline (token_list_to_string response)
    | None -> print_endline "..."
    with Failure _ -> print_endline "...");
    print_endline "" ;;

let bot_converse = () ;;

(* Print glorious banner *)
let () =
  print_endline "\n\nWelcome to...";
  let banner = open_in "obabble_art.txt" in
  (try while true do
    print_endline (input_line banner)
  done with End_of_file -> ());
  print_endline "\n" ;;

(* Run conversation loop *)
let () =
  let history = ref [] in 
  print_endline "Begin a conversation:";
  while true do
    print_string "|: ";
    let query = string_to_token_list (read_line ()) in
    history := slice cHISTORY (query @ !history);
    List.iter (query_bot query history) bots;
  done ;;

