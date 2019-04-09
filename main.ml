open ANSITerminal

let play_game =
  failwith "Unimplemented"

let main () = 
  ANSITerminal.(print_string[red] "\n\ Welcome to Snake! Press enter to start. 
    \n");
  match read_line () with
  | exception End_of_file -> ()
  | _ -> play_game 

let () = main ()