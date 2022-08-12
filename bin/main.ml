
open Learn_ocaml

let int_lst = [1; 2; 3; 4]
let lst = ["maya"; "hee"; "maya"; "hoo"]
let ext_str s = match s with Some s -> s | None -> ""

let () = 
  print_endline "Hello, World!";

  print_string ("This " ^ "is " ^ "a " ^ "string");
  print_endline "";

  print_string (String.concat " " ("This is an array:" :: lst));
  print_endline "";
  
  print_string ("Here is the last element: " ^ ext_str (Excersises.last lst));
  print_endline "";

  print_string ("Here is the 2nd element: " ^ ext_str (Excersises.nth (lst, 1)));
  print_endline "";
  
  let (x, y) = Leet.two_sum (int_lst @ [0]) 1 in
   print_string ("Here is a two_sum solution: (" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")");
  print_endline "";

  print_endline "Goodbye, World!"
