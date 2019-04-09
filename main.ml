open ANSITerminal

let play_game ()=
  print_endline "Hi"

let main () = 
  ANSITerminal.(print_string[red] "\n\ Welcome to Snake! Press enter to start \n");
  print_string[red] "> ";
  match read_line () with
  | exception End_of_file -> ()
  | x -> play_game ()

let () = main ()