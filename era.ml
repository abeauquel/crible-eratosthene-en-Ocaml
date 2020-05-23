(* fonction d'affichage de la liste *)
let rec print_int_list = function 
    [] -> ()
  | int::l -> print_int int ; print_string "; "; print_int_list l;;


(* creation d'une liste en fonction d'un interval *) 
let create_list a b =
  let rec aux a b =
    if a > b then [] else a :: aux (a+1) b  in
  if a > b then List.rev (aux b a) else aux a b;;




(* filtrer une liste en fonction d'un predicat *) 
let rec filter_out g = function
  | [] -> []
  | a::l when g a ->
      let l1 = filter_out g l in
      a::l1
  | a::l -> filter_out g l ;;

(* Verifier si x est multiple de m et x!=m *) 
let is_multiple x m = 
  if x=m then
    false
  else 
    match x mod m with
      0 -> true
    | _ -> false;;

(*Suprression des multiples d'une liste*)
let remove_multiple n list=filter_out (function m -> (is_multiple m n)=false ) list;;

(* fonction auxilière du crible d'eratosthene*)
let rec aux max n list = match (List.nth list n * List.nth list n ) < max with
    true -> aux max (n+1)(remove_multiple (List.nth list n) list) 
  | false -> list;;

(* fonction du crible d'eratosthene*)
let sieve max = aux max 0 (create_list 2 max);;

sieve 1000
