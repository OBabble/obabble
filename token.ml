open Str ;;
open String;;

type token =
  | Start
  | Word of string
  | End ;;

let tokenize (s : string) : string list =
  let p = Str.regexp "\\([\\.!?,]+\\)" in
  Str.split (regexp "[ \t]+")
            (Str.global_replace p " \\1 " (lowercase_ascii s)) ;;

let grammarize (t : token) : string =
  match t with
  | Start -> ""
  | End -> ""
  | Word "!" -> "!"
  | Word "." -> "."
  | Word "?" -> "?"
  | Word "," -> ","
  | Word w -> " " ^ w ;;

let token_to_string (t : token) : string =
  match t with
  | Start -> ""
  | End -> ""
  | Word w -> w ;;

let token_list_to_string (t : token list) : string =
  List.fold_left (fun a x -> a ^ x) "" (List.map grammarize t) ;;

let token_list_to_string_list (t : token list) : string list =
  List.map token_to_string t ;;

let token_list (s : string) : token list =
  List.map (fun x -> Word x) (tokenize s) ;;

let string_list_to_token_list (s : string list) : token list =
  List.map (fun x -> Word x) s ;;
