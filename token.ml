open Str ;;
open String;;

type token =
  | Word of string
  | End ;;

let tokenize (s : string) : string list =
  let p = Str.regexp "\\([\\.!?,]+\\)" in
  Str.split (regexp "[ \t]+")
            (Str.global_replace p " \\1 " (lowercase_ascii s)) ;;

let grammarize (t : token) : string =
  match t with
  | End -> ""
  | Word "!" -> "!"
  | Word "." -> "."
  | Word "?" -> "?"
  | Word "," -> ","
  | Word w -> " " ^ w ;;

let token_to_string (t : token) : string =
  match t with
  | End -> ""
  | Word w -> w ;;

let rec token_list_to_string (t : token list) : string =
  List.fold_left (fun a x -> a ^ x) "" (List.map grammarize t) ;;

let token_list_to_string_list (t : token list) : string list =
  List.map token_to_string t ;;
