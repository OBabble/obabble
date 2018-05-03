(* O[B]abble Chatroom - A Bot Chatroom for O[B]abble *)
(* Copyright (c) 2018 The OBabble Team *)

open Token ;;
open Markov ;;

type bot_spec = {name : string; corpus : string} ;;

let bots : bot_spec list = [
  {name = "chad"; corpus = "chad.txt"};
  {name = "mary"; corpus = "mary.txt"};
] ;;

class type chatroom_t =
  object
    method load : bot_spec list -> unit
    method run : unit -> unit
  end ;;

class chatroom : chatroom_t =
  object
    val mutable bots : Model.model list = []
    
    metho load (l : bot_spec list) : unit =
      bots <- List.map (fun s ->
        let model = new Model.model s.name 0 in
        print_string "Loading from saved model...";
        (if not (model#load s.name) then
          (print_string "Save not found.";
           print_endline "Training new model...";
           model#train 1000000 (Parser.get_stream s.corpus);
           model#save s.name)
        else print_endline "Done!");
        model) l

    method run () : unit =
      (* Print glorious banner *)
      let () =
        print_endline "\n\nWelcome to...";
        let banner = open_in "obabble_art.txt" in
        (try while true do
          print_endline (input_line banner)
        done with End_of_file -> ());
        print_endline "\n" in

      (* Start bots into different thread *)

      (* Start user prompt *)
      (*

      let () =
        print_endline "Begin a conversation:";
        while true do
          try
            print_string "|: ";
            let seed = read_line () in
            print_string "|> ";
            (* print_endline (seed ^ " " ^ (Generator.gen model
                                        (Parser.get_user_token_list seed))) *)
            print_endline (seed ^ " " ^ (Generator.gen model.chains
                                        (Word seed)))
          with Generator.WordNotFound _ -> print_endline "..."
        done ;; *)
  end
