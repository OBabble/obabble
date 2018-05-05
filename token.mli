(* O[B]abble - A Markov Chain Chatbot in OCaml *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Token Module --
 * Definitions and tools for working with tokens.
 *)

open Str ;;
open String;;

type token =
  | Start
  | Word of string
  | End ;;

val tokenize : string -> string list ;;
val grammarize : token -> string ;;
val token_to_string : token -> string ;;
val token_list_to_string : token list -> string ;;
val token_list_to_string_list : token list -> string list ;;
val string_to_token_list : string -> token list ;;
val string_list_to_token_list : string list -> token list ;;
