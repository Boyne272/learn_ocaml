(*
   Some example cases to help learn ocaml.
   No particular use of any of these.
*)

type number = Int of int | Float of float

let add_one (x : number) =
  match x with Int i -> Int (i + 1) | Float f -> Float (f +. 1.)

(* Following tests add_one *)
let%test "add_one int" = add_one (Int 1) = Int 2
let%test "add_one flt" = add_one (Float 1.) = Float 2.

(* Following tests last from the excersises.ml *)
let%test "test other file" = Excersises.last [] = None

(* Following tests that testing is working *)
let%test "always fail" = false
let%test "always pass" = true

