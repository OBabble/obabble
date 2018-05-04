(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * Parser --
 * Tools for parsing and reading in training data.
 *)

open Scanf;;
open String;;
open Str;;
open Token ;;

(* Given a line-by-line string stream, converts to a token stream *)
let input_stream (s : string Stream.t) : token Stream.t =
  let l = ref None in
  let get_string (s : string Stream.t) : token =
    (match !l with
     | None -> l := Some (tokenize (Stream.next s)); Start
     | Some [] -> l:= None; End
     | Some (hd :: tl) -> l := Some tl; Word hd)
  in
  Stream.from (fun _ -> Some (get_string s)) ;;

(* Obtains a line-by-line string stream from input file *)
let get_stream (filename : string) : token Stream.t =
  let in_channel = open_in filename in
  let string_stream = Stream.from (fun _ -> try
                                              Some (input_line in_channel)
                                            with
                                              End_of_file -> None)
  in
  input_stream string_stream ;;
