open Scanf;;
open String;;
open Str;;

open Token ;;

module type PARSER =
  sig
    val get_stream : string -> token Stream.t
  end ;;

module Parser : PARSER =
  struct

    let tokenize (s : string) : string list =
      let p = Str.regexp "\\([\\.!?,]+\\)" in
      Str.split (regexp "[ \t]+") (Str.global_replace p " \\1 " s)

    let input_stream (s : string Stream.t) : token Stream.t =
      let l = ref None in
      let rec get_string (s : string Stream.t) : token =
        (match !l with
         | None -> l := Some (tokenize (Stream.next s)); get_string s
         | Some [] -> l:= None; End
         | Some (hd :: tl) -> l := Some tl; Word (lowercase_ascii hd))
      in
      Stream.from (fun _ -> Some (get_string s))

    let get_stream (s : string) : token Stream.t =
      let in_channel = open_in s in
      let string_stream = Stream.from (fun _ -> try
                                                  Some (input_line in_channel)
                                                with
                                                  End_of_file -> None)
      in
      input_stream string_stream
  end ;;
