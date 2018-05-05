(* O[B]abble - A Markov Chain Chatbot in OCaml *
 * Copyright (c) 2018 The OBabble Team
 *
 * The Model Class --
 * An object to represent the chatbot, with training and
 * generator utilities
 *)

open Token ;;
open Markov ;;

type model_t = {chains : mchain; assocs : mchain}  ;;

class type model_class_t =
  object
    method name : string
    method train : int -> token list Stream.t -> unit
    method save : string -> unit
    method load : string -> bool
    method chains : mchain
    method assocs : mchain
    method query : token list -> int -> int -> float -> token list option
    method set_debug : bool -> unit
  end ;;

class model : string -> model_class_t ;;
