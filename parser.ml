open Scanf;;
open NativeLazyStreams;;
open String;;

(* input token types : words, punctuation, end of sentence, and end of file *)
type token =
  | End
  | Word of string
  | Punct of string
  | Eof

class type parser_t =
object
  method tokenize : string -> string list
  method get_string : unit -> string
  method input_stream : unit -> string Stream.t
  method token_stream : unit -> token Stream.t
end ;;


class parser (s : string) : parser_t =
object(this)

  val mutable input_list : string list ref = ref []

  val mutable lines : string Stream.t =
    let in_channel = open_in s in
    Stream.from (fun _ -> try Some (input_line in_channel) with End_of_file -> None)

  method tokenize (line : string) : string list = String.split_on_char ' ' line

  method get_string () : string =
    match !input_list with
    | [] -> input_list := tokenize (Stream.next lines); this#get_string()
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
    |
    |

  method token_stream () : token Stream.t =
    stream_map (fun x -> ) input_stream()

end;;

let test = new parser "test.txt";;
let stream = test#input_stream();;

open Str ;;

let tokenize (line : string) : string list = Str.split (regexp "[ \t]+") (Str.global_replace (regexp "[[:punct:]]") " " line);;





