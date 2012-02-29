#use "projet.ml"

let rec dev_build_ens(l : int list) = 
  match l with
    | [] -> NIL
    | h::q -> Cons(h, dev_build_ens q);;
