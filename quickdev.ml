
let rec dev_build_ens(l : 'e list) = 
  match l with
    | [] -> NIL
    | h::q -> Cons(h, dev_build_ens q);;

let rec dev_build_list(ens) =
  match ens with
    | NIL -> []
    | Cons(i, e) -> i::dev_build_list(e);;

(* Aliases *)
let dbe(l) = dev_build_ens l;;
let dbl(e) = dev_build_list e;;
