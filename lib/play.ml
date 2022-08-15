(*
   Some example cases to help learn ocaml.
   no particular use of any of these.
*)

(* custom type *)
type number = Int of int | Float of float

(* simple function *)
let add_one (x : number) =
  match x with Int i -> Int (i + 1) | Float f -> Float (f +. 1.)

(* following tests add_one *)
let%test "add_one int" = add_one (Int 1) = Int 2
let%test "add_one flt" = add_one (Float 1.) = Float 2.

(* following tests last from the excersises.ml *)
let%test "test other file" = Excersises.last [] = None

(* following tests that testing is working *)
let%test "always fail" = false
let%test "always pass" = true

(* some exception *)
exception Empty_list

(* raising exception & match function *)
let head = function
    [] -> raise Empty_list
  | hd :: _ -> hd

(* exception catching with try *)
let opt_head lst = try (head lst) with Empty_list -> "ha ha"
let default_head lst def = 
  match (head lst) with
    exception Empty_list -> def
  | v -> v

let l = ["maya"; "hee"; "maya"; "hoo";]
let%test "test head" = head l = "maya"
let%test "test head error" = try head [] with Empty_list -> true
let%test "test opt_head" = opt_head [] = "ha ha"
let%test "test opt_head" = opt_head l = "maya"
let%test "test default_head" = default_head l "maya ha ha" = "maya"
let%test "test default_head" = default_head [] "maya ha ha" = "maya ha ha"
