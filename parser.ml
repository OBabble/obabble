open Scanf;;

type reader = {read_next : unit -> string option};;

let make_reader file_name =
  let in_channel = open_in file_name in
  let closed = ref false in
  let input = Scanning.from_channel in_channel in
  let read_next_line = fun () ->
    if !closed then
      None
    else
      try
        Some (Scanf.bscanf input "%[^\r\n]\n" (fun x -> x))
      with
        End_of_file ->
          let _ = close_in_noerr in_channel in
          let _ = closed := true in
          None in
  {read_next = read_next_line};;

let r = make_reader "test.txt";;
let next_line = r.read_next ();;


type token =
  | End
  | Word of string
  | Punct of string


























