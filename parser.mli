open Scanf;;
open String;;
open Str;;

type token

module type PARSER =
  sig
    val get_stream : string -> token Stream.t
  end ;;
