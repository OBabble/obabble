(* O[B]abble Chatroom - A Bot Chatroom for O[B]abble *)
(* Copyright (c) 2018 The OBabble Team *)

let debug = false ;;

let () = Random.self_init () ;;

let cMAXTRAIN = 1000 ;;
let cSAMPLES = 20 ;;
let cMAXLENGTH = 15 ;;
let cTHRESHOLD = 10. ;;
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

let output = open_out "output.txt" ;;

(* Print glorious banner *)
let () =
  print_endline "\n\nWelcome to...";
  let banner = open_in "obabble_art_rich.txt" in
  (try while true do
    let line = input_line banner in
    print_endline line;
    Printf.fprintf output "%s\n%!" line;
  done with End_of_file -> ());
  Printf.fprintf output "\n\n%!";
  print_endline "\n" ;;

let babble_bot (bot, query, history, f, m : 
  Model.model * token list ref * token list ref * out_channel * Mutex.t): unit =
  while true do
    Thread.delay (Random.float 10.0);
    Mutex.lock m;
    (try match bot#query !query !history cSAMPLES cMAXLENGTH cTHRESHOLD with
    | Some response -> 
        history := slice cHISTORY (!query @ !history);
        query := response;
        Printf.fprintf f "\027[34;1m|\027[37m%s\027[34m|>\027[0m %s\n%!" 
          bot#name (token_list_to_string response);
    | None -> ()
    with Failure e -> Printf.fprintf f "|%s|> Error: Failure%s\n%!" bot#name e);
    Mutex.unlock m;
  done ;;

(* Run conversation loop *)
let () =
  let query = ref [] in
  let history = ref [] in 
  let mutex = Mutex.create () in
  
  List.iter (fun b ->
    ignore (Thread.create babble_bot (b, query, history, output, mutex)))  
  bots;

  print_endline "Open `tail -f output.txt` to see the conversation!";
  print_endline "Begin by giving input here:";
  while true do
    print_string "|: ";
    let line = read_line () in
    Mutex.lock mutex;
    Printf.fprintf output "\027[31;1m|\027[37mYOU\027[31m|>\027[0m %s\n%!" line;
    let split = string_to_token_list line in
    if List.length split > 0 then
      (query := split;
      history := split);
    Mutex.unlock mutex;
  done ;;

