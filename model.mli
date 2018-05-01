open Token ;;
open Markov ;;

type model = {assocs : mchain; chain : mchain} ;;

val empty : unit -> model ;;
val train : model -> int -> int -> token Stream.t -> unit ;;
val save : model -> string -> unit ;;
val load : string -> model ;;
