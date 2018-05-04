(* O[B]abble Chatroom - A Bot Chatroom for O[B]abble *)
(* Copyright (c) 2018 The OBabble Team *)

let debug = false ;;

let () = Random.self_init () ;;

let cMAXTRAIN = 1000 ;;
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
  {name = "hal"; corpus = "2001.txt"};
  {name = "catwoman"; corpus = "catwoman_lines.txt"};
  {name = "magneto"; corpus = "magneto_xmen.txt"};
  {name = "tinman"; corpus = "tinman_lines.txt"};
] ;;

let model_from_spec (s : bot_spec) : Model.model =
  let model = new Model.model s.name in
  Printf.printf "\nPreparing bot \"%s\"...\n%!" s.name;
  Printf.printf "Loading from saved model...\n%!";
  (if not (model#load s.name) then
    (Printf.printf "Save not found.\n%!";
     print_endline "Training new model...";
     model#train cMAXTRAIN (Parser.get_stream s.corpus);
     model#save s.name)
  else Printf.printf "Model ready!\n%!");
  model#set_debug debug;
  model ;;

let bots = List.map model_from_spec bot_specs ;;
let bots_array = Array.of_list bots ;;

let bot_converse () = () ;;

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
  let query = ref [] in
  let history = ref [] in 
  print_endline "Begin a conversation:";
  while true do
    print_string "|: ";
    query := string_to_token_list (read_line ());
    history := slice cHISTORY (!query @ !history);
    
    print_endline "";

    while Random.float 1.0 > 0.18 do
      Thread.delay (Random.float 1.8);
      let bot = bots_array.(Random.int (Array.length bots_array)) in
      Printf.printf "|%s|> %!" bot#name;
      (try match bot#query !query !history cSAMPLES cMAXLENGTH cTHRESHOLD with
      | Some response -> 
          history := slice cHISTORY (!query @ !history);
          query := response;
          print_endline (token_list_to_string response);
      | None -> print_endline "..."
      with Failure _ -> print_endline "...");
    done;
    print_endline "";
  done ;;

