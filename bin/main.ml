
open Learn_ocaml

let lst = ["maya"; "hee"; "maya"; "hoo"]
let final = Excersises.last lst
let final_as_str = match final with Some v -> v | None -> ""

let () = 
  print_endline "Hello, World!";

  print_string ("This " ^ "is " ^ "a " ^ "string");
  print_endline "";

  print_string (String.concat " " ("This is an array:" :: lst));
  print_endline "";
  
  print_string ("Here is the last element: " ^ final_as_str);
  print_endline "";

  print_endline "Goodbye, World!"
