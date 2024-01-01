let rec last = function
| [] -> None
| [x] -> Some x
| _ :: t -> last t;;

last ["a"; "b"; "c"; "d"];;
last ["a"];;
last [];;

let rec last_two = function
| [] | [_] -> None
| [x ; y] -> Some (x,y)
| _ :: t -> last_two t;;

last_two ["a"; "b"; "c"; "d"];;
last_two ["a"];;

let rec nth n = function
| [] -> None
| h :: t -> if n = 0 then Some h else nth (n - 1) t;;

nth 5 ["a"; "b"; "c"; "d"; "e"; "f"];;

let length lst = 
   let rec aux acc = function 
   | [] -> acc
   | _ :: t -> aux (acc + 1) t
   in
   aux 0 lst;;

length ["a"; "b"; "c"];;
length [];;


let rec rev = function
| [] -> []
| [x] -> [x]
| h :: t -> rev t @ [h];;

 rev ["a"; "b"; "c"];;


let is_palindrome lst = rev lst = lst;;

is_palindrome ["x"; "a"; "m"; "a"; "x"];;

type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let flatten lst = 
  let rec aux acc = function
  | [] -> acc
  | One x :: y -> aux (acc @ [x]) y
  | Many x :: y -> aux (aux acc  x)  y
  in
  aux [] lst;;

flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;


let compress lst =
  let rec aux acc = function
  | [] -> acc
  | h :: t when List.length acc = 0 -> aux [h] t
  | h :: t -> if List.hd acc = h then aux acc t else aux (h :: acc) t
  in
  List.rev (aux [] lst);;

compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

let rec duplicate  = function
  | [] -> []
  | h :: t -> [h; h] @ duplicate t;;

duplicate ["a"; "b"; "c"; "c"; "d"];;

let replicate lst n = 
  let rec aux rep = function
  | [] -> []
  | h :: t as x -> if rep > 0 then h :: aux (rep - 1) x else aux n t
  in
  aux n lst;;

replicate ["a"; "b"; "c"] 3;;


let drop lst n = 
  let rec aux rep acc = function
  | [] -> acc
  | h :: t -> if rep = 0 then aux (n - 1) acc t else aux (rep - 1) (acc @ [h]) t in
  aux (n - 1) [] lst;;

drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;


type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec insert tree x = match tree with 
| Empty -> Node (x, Empty, Empty)
| Node (y, l, r) -> if y = x then tree
                  else if y < x then  Node (y, insert l x, r)
                  else Node (y, l, insert r x);;

let construct lst = List.fold_left insert Empty lst;;

construct [5; 3; 18; 1; 4; 12; 21];;

let rec is_mirror lft rght = 
match lft, rght with
| Empty, Empty -> true
| Node(_, l1, r1), Node(_, l2, r2) -> is_mirror l1 l2 && is_mirror r1 r2
| _ -> false;;

let is_symmetric = function
| Empty -> true
| Node(_, l, r) -> is_mirror l r;;

is_symmetric (construct [5; 3; 18; 1; 4; 12; 21]);;
not (is_symmetric (construct [3; 2; 5; 7; 4]));;

let rec count_leaves = function
| Empty -> 0
| Node(_, Empty, Empty) -> 1
| Node(_, l, r) -> count_leaves l + count_leaves r;;

count_leaves Empty;;
count_leaves (construct [5; 3; 18; 1; 4; 12; 21]);;



