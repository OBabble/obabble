(* O[B]abble - A Markov Chain Chatbot in Ocaml *)
(* Copyright (c) 2018 The OBabble Team *)

let _debug = true ;;

let cMAXTRAIN = (int_of_float 1e5) ;;
let cSAMPLES = (int_of_float 1e3) ;;
let cMAXLENGTH = 10 ;;

open Token ;;
open Markov ;;

let corpus = "movie_lines.txt" ;;
let model_name = "movie_lines" ;;

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [] ;;

let cSTOPWORDS = string_list_to_token_list (read_lines "stop.txt") ;;

let model = new Model.model model_name 1 ;;

(* Initialize model *)
let () =
  Printf.printf "Loading from saved model...%!";
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


let () =
  print_endline "Begin a conversation:";
  while true do
    print_string "|: ";
    let seed = read_line () in
    print_string "|> ";
    try
      let seed_tokens = token_list seed in
      let seed_pool = List.concat (List.map
        (Generator.roll_n cSAMPLES model#assocs) (Generator.stop_filter seed_tokens)) in
      let candidates = List.map (fun s ->
        s :: (Generator.gen cMAXLENGTH model#chains s)) seed_pool in
      let scored = Generator.score model#assocs seed_tokens candidates in
      if _debug then (List.iter (fun (l, s) ->
        print_endline "";
        print_endline (token_list_to_string l);
        Printf.printf "-->(score %f)\n" s) scored);
      let best, score = List.hd
        (List.sort (fun (_, a) (_, b) -> compare b a) scored) in
      if _debug then (print_endline "\n\n\n";
      Printf.printf "(score: %f)\n" score);
      print_endline (token_list_to_string best);
    with Failure _ -> print_endline "..."
  done ;;
