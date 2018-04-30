open Scanf ;;
open String ;;
open Str ;;

open Token ;;

module type PARSER =
  sig
    val get_stream : string -> token Stream.t
  end ;;
