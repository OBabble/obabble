(*

TODO:

improve tokenize

 *)

open Scanf;;
open String;;
open Str;;

type token =
  | End
  | Word of string

module type PARSER =
  sig
    val get_stream : string -> token Stream.t
  end ;;

module Parser : PARSER =
  struct

    let tokenize (line : string) : string list =
      let p = Str.regexp "[.]+" in
      let e = Str.regexp "[!]+" in
      let q = Str.regexp "[?]+" in
      let c = Str.regexp "[,]+" in
      let sq = Str.regexp "[\']+" in
      let dq = Str.regexp "[\"]+" in
      let p_line = Str.global_replace p " ." line in
      let e_line = Str.global_replace e " !" p_line in
      let q_line = Str.global_replace q " ?" e_line in
      let c_line = Str.global_replace c " ," q_line in
      let sq_line = Str.global_replace sq " \' " c_line in
      let dq_line = Str.global_replace dq " \" " sq_line in
      Str.split (regexp " +") dq_line

    let input_stream (s : string Stream.t) : token Stream.t =
      let l = ref None in
      let rec get_string (s : string Stream.t) : token =
        (match !l with
         | None -> l := Some (tokenize (Stream.next s)); get_string s
         | Some [] -> l:= None; End
         | Some (hd :: tl) -> l := Some tl; Word hd)
      in
      Stream.from (fun _ -> Some (get_string s))

    let get_stream (s : string) : token Stream.t =
      let in_channel = open_in s in
      let string_stream = Stream.from (fun _ -> try Some (input_line in_channel) with End_of_file -> None) in
      input_stream string_stream
  end ;;

let test = Parser.get_stream "test2.txt";;
