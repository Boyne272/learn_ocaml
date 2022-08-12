(* https://leetcode.com/problems/two-sum/ *)

exception Empty_list
let two_sum list targ =
  let tbl = Hashtbl.create ~random:true (Excersises.length list) in
  let rec aux = function
    | [] -> raise Empty_list 
    | h :: t -> 
      if (Hashtbl.find_opt tbl h != None) 
        then (Hashtbl.find tbl h, Hashtbl.length tbl) else begin 
          Hashtbl.add tbl (targ - h) (Hashtbl.length tbl);
          aux t
      end
  in aux list

let%test "simple case 1" = two_sum [2; 7; 11; 15] 9 = (0, 1)
let%test "simple case 2" = two_sum [3; 2; 4] 6 = (1, 2)
let%test "simple case 3" = two_sum [3; 3] 6 = (0, 1)
let%test "long case 1" = two_sum [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 0] 1 = (0, 12)
let%test "long case 2" = two_sum [-1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; -1] (-2) = (0, 14)
let%test "long case 3" = two_sum [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; -1] 0 = (0, 12)
