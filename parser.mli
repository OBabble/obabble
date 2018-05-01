open Scanf ;;
open String ;;
open Str ;;
open Token ;;

module type PARSER =
  sig
    val get_stream : string -> token Stream.t
    val get_user_token_list : string -> token list
  end ;;

module Parser : PARSER ;;
