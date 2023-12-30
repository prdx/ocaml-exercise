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
  
