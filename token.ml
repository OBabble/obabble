(* O[B]abble - An OCaml Chat Bot *
 * Copyright (c) 2018 The OBabble Team
 *
 * Token --
 * Definitions and tools for working with tokens.
 *)

open Str ;;
open String;;

type token =
  | Start
  | Word of string
  | End ;;

(* Splits a string between words and punctuation marks into a string list *)
let tokenize (s : string) : string list =
  let p = Str.regexp "\\([\\.!?,]+\\)" in
  Str.split (regexp "[ \t]+")
            (Str.global_replace p " \\1 " (lowercase_ascii s)) ;;

(* Receives a token and makes sure when converting from a token to a string that
 * the output string is 'grammatically' correct (spaces around punctuation)
 *)
let grammarize (t : token) : string =
  match t with
  | Start -> ""
  | End -> ""
  | Word "!" -> "!"
  | Word "." -> "."
  | Word "?" -> "?"
  | Word "," -> ","
  | Word w -> " " ^ w ;;

(* Receives a token and converts to a string without consideration for
 * grammatical correctness
 *)
let token_to_string (t : token) : string =
  match t with
  | Start -> ""
  | End -> ""
  | Word w -> w ;;

let token_list_to_string (t : token list) : string =
  List.fold_left (fun a x -> a ^ x) "" (List.map grammarize t) ;;

let token_list_to_string_list (t : token list) : string list =
  List.map token_to_string t ;;

let string_to_token_list (s : string) : token list =
  List.map (fun x -> Word x) (tokenize s) ;;

let string_list_to_token_list (s : string list) : token list =
  List.map (fun x -> Word x) s ;;
