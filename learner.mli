open Token ;;
open Markov ;;

val train : mchain -> token Stream.t -> unit
val save : mchain -> string -> unit
val load : string -> mchain
