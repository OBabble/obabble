(* O[B]abble - A Markov Chain Chatbot in OCaml *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Parser Module --
 * Tools for parsing and reading in training data.
 *)

open Scanf ;;
open String ;;
open Str ;;
open Token ;;

(* Given a line-by-line string stream, converts to a token list stream *)
let input_stream (s : string Stream.t) : token list Stream.t =
  Stream.from (fun _ -> 
    Some (Start :: (string_to_token_list (Stream.next s)) @ [End])
  ) ;;

(* Obtains a line-by-line string stream from input file *)
let get_stream (filename : string) : token list Stream.t =
  let in_channel = open_in filename in
  let string_stream = Stream.from 
    (fun _ -> try Some (input_line in_channel)
              with End_of_file -> None)
  in
  input_stream string_stream ;;
