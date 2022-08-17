(*
  Test problems for learning the language 
  see https://ocaml.org/problems 
*)


(* Tail of a list *)
let rec last l =
  match l with [] -> None | h :: t -> if t = [] then Some h else last t;;

let%test "last happy" = last ["a" ; "b" ; "c" ; "d"] = Some "d"
let%test "last empty" = last [] = None


let first = function [] -> None | h :: _ -> Some h;;

let%test "first happy" = first ["a" ; "b" ; "c" ; "d"] = Some "a"
let%test "first empty" = first [] = None


(* Last two elements of a list *)
let rec last_two l =
  match l with
  | [] -> None
  | h1 :: t1 -> (
      match t1 with
      | [] -> None
      | h2 :: t2 -> if t2 == [] then Some (h1, h2) else last_two t1);;

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
(* let rec nth lst i = 
  match lst, i with
  ([], _) -> None
  | (h :: _, 0) -> Some h
  | (_ :: t, _) -> nth t (i-1) *)
let rec nth = function
  ([], _) -> None
  | (h :: _, 0) -> Some h
  | (_ :: t, i) -> nth (t, (i-1));;

let%test "nth happy" = nth (["a"; "b"; "c"; "d"; "e"], 2) = Some "c"
let%test "nth empty" = nth ([], 2) = None
let%test "nth short" = nth (["a"], 2) = None


(* Length of a list *)
let rec length lst = 
  match lst with
  | [] -> 0
  | _ :: t -> 1 + length t

let%test "length happy" = length ["a"; "b"; "c"] = 3
let%test "length empty" = length [] = 0


(* Reverse a list *)
let rec rev lst = 
  match lst with
  | [] -> []
  | h :: t -> rev t @ [h];;

let%test "reverse happy" = rev ["a"; "b"; "c"] = ["c"; "b"; "a"]
let%test "reverse empty" = rev [] = []


(* Palindrome *)
let is_palindrome lst = rev lst = lst;;

let%test "is_palindrome pass" = is_palindrome ["x"; "a"; "m"; "a"; "x"] = true
let%test "is_palindrome fail" = is_palindrome ["a"; "b"] = false
let%test "is_palindrome empty" = is_palindrome [] = true


(* Flatten a list *)
type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let rec flatten_first = function
  [] -> []
  | h :: t -> (
    match h with
    | One o -> [o] @ flatten_first t
    | Many m -> (flatten_first m) @ (flatten_first t)
  );;

let rec flatten = function
  | [] -> []
  | One o :: t -> [o] @ flatten t
  | Many m :: t -> (flatten m) @ (flatten t);;

let flatten_model list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  List.rev (aux [] list);;

let node_list = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]
let%test "flatten single" = flatten [One "a"] = ["a";]
let%test "flatten double" = flatten [Many [One "a"; One "b"]] = ["a"; "b"]
let%test "flatten happy" = flatten node_list = ["a"; "b"; "c"; "d"; "e"]


(* Eliminate duplicates *)

module SS = Set.Make(String);;
let compress_proper lst = (
   let set = SS.of_list lst in
   List.of_seq (SS.to_seq set)
);;

let compress = function
  [] -> [] | first :: rest -> (
  let rec aux acc prev = function
    [] -> acc
    | h :: t -> aux (if h = prev then acc else acc @ [h]) h t
  in aux [first] first rest
);;

let rec compress_model = function
    | a :: (b :: _ as t) -> if a = b then compress_model t else a :: compress_model t
    | smaller -> smaller;;

let%test "compress happy" = compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"]
let%test "compress_model happy" = compress_model ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"]
let%test "compress_proper happy" = compress_proper ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "d"; "e"]
