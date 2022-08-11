(*
  Test problems for learning the language 
  see https://ocaml.org/problems 
*)


(* Tail of a list *)
let rec last l =
  match l with [] -> None | h :: t -> if t = [] then Some h else last t

let%test "last happy" = last ["a" ; "b" ; "c" ; "d"] = Some "d"
let%test "last empty" = last [] = None


(* Last two elements of a list *)
let rec last_two l =
  match l with
  | [] -> None
  | h1 :: t1 -> (
      match t1 with
      | [] -> None
      | h2 :: t2 -> if t2 == [] then Some (h1, h2) else last_two t1)

(* 
let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x,y)
  | _ :: t -> last_two t
*)
      
let%test "last_two happy" = last_two ["a" ; "b" ; "c" ; "d"] = Some ("c", "d")
let%test "last_two empty" = last_two [] = None
let%test "last_two short" = last_two ["a"] = None


(* N'th element of a list *)
let rec nth lst i = 
  match lst, i with
  ([], _) -> None
  | (h :: _, 0) -> Some h
  | (_ :: t, _) -> nth t (i-1)

let%test "nth happy" = nth ["a"; "b"; "c"; "d"; "e"] 2 = Some "c"
let%test "nth empty" = nth [] 2 = None
let%test "nth short" = nth ["a"] 2 = None