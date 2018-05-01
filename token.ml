open Str ;;

type token =
  | Word of string
  | End ;;

let tokenize (s : string) : string list =
  let p = Str.regexp "\\([\\.!?,]+\\)" in
  Str.split (regexp "[ \t]+")
            (Str.global_replace p " \\1 " (lowercase_ascii s)) ;;
