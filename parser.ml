open Scanf;;
open String;;
open Str;;

(* input token types : words, punctuation, end of sentence, and end of file *)
type token =
  | End of string
  | Word of string
  | Punct of string
  | Eof

class type parser_t =
object
  method tokenize : string -> string list
  method get_string : unit -> string
  method input_stream : unit -> string Stream.t
  method token_stream : unit -> token Stream.t
  method stream_map : (string -> token) -> string Stream.t -> token Stream.t
  method token_of_string : string -> token
end ;;


class parser (s : string) : parser_t =
object(this)

  val mutable input_list : string list ref = ref []

  val mutable lines : string Stream.t =
    let in_channel = open_in s in
    Stream.from (fun _ -> try Some (input_line in_channel) with End_of_file -> None)

  method tokenize (line : string) : string list =
    let p = Str.regexp "[.]+" in
    let e = Str.regexp "[!]+" in
    let q = Str.regexp "[?]+" in
    let c = Str.regexp "[,]+" in
    let p_line = Str.global_replace p " ." line in
    let e_line = Str.global_replace e " !" p_line in
    let q_line = Str.global_replace q " ?" e_line in
    let c_line = Str.global_replace c " ," q_line in
    Str.split (regexp " +") c_line

  method get_string () : string =
    match !input_list with
    | [] -> input_list := this#tokenize (Stream.next lines); this#get_string()
    | hd :: tl -> input_list := tl; hd

  method input_stream () : string Stream.t =
    Stream.from (fun _ -> Some (this#get_string()))

  method stream_map f stream =
    let rec next i =
      try Some (f (Stream.next stream))
      with Stream.Failure -> None in
    Stream.from next

  method token_of_string (s : string) : token =
    match s with
    | "." -> End s
    | "!" -> End s
    | "?" -> End s
    | "," -> Punct s
    | "\"" -> Punct s
    | "'" -> Punct s
    | _ -> Word s

  method token_stream () : token Stream.t =
    this#stream_map this#token_of_string (this#input_stream())

end;;


(* Usage *)

(*
Detailed Usage Instructions

May need to use ' #load "str.cma" ;; in interpreter'

# #use "parser.ml" ;;

# let test = new parser "test.txt";; - make new parser object from 'test.txt'
# let stream = test#token_stream();; - makes token stream from parser object
# Stream.next stream ;; - gives next token in token stream (repeat multiple times to get the next token in the stream)

*)


(*

Simplified Usage Instructions

#load "str.cma" ;;
#use "parser.ml" ;;
let test = new parser "test.txt";;
let stream = test#token_stream();;
Stream.next stream ;;

 *)
