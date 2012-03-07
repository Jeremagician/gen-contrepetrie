
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

let rec dev_build_multi_ens(l) =
  match l with
    | [] -> VIDE
    | (a,b)::c -> Add((a,b), dev_build_multi_ens c);;

let rec dev_build_multi_list(ens : 'e multiens) =
  match ens with
    | VIDE -> []
    | Add((el,count), seq) -> (el,count)::dev_build_multi_list seq;;

let dbme(l) = dev_build_multi_ens l;;
let dbml(e) = dev_build_multi_list e;;
