open Str ;;

let f = open_in "movie_lines_ordered.txt" ;;

let () =
  try
    let prev = ref None in
    while true do
      let line = input_line f in
      let m = string_match 
        (regexp ".+\\$\\+\\+\\+ (.+) \\+\\+\\+$.+\\$\\+\\+\\+ (.+) \\+\\+\\+$\\+\\+\\+")
        line 0 in
      if m then print_endline "YES"
      else print_endline "NO"
    done
  with End_of_file -> () ;;

