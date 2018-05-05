(* O[B]abble - A Markov Chain Chatbot in OCaml *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Parser Module --
 * Tools for parsing and reading in training data.
 *)

open Token ;;

val get_stream : string -> token list Stream.t
