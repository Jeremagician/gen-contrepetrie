(* Projet INF121 : Ensemble, Multi-ensembles & Contrepets

           ENJOLRAS Clement - DERDAELE Jeremy


PARTIE I - Ensembles 
*)

(* Commencons par definir le type ensemble *)
type 'e ensemble = NIL | Cons of 'e * 'e ensemble;;

(* ainsi que l'ensemble vide *)
let ensemblevide = NIL;;

(* Temporary for quick dev *)
#use "quickdev.ml";;

(* Specification : *)
let estvide (e: 'e ensemble) : bool = e = ensemblevide;;

let rec cardinal(e: 'e ensemble) : int = match e with
  | NIL -> 0
  | Cons(_, seq) -> 1 + cardinal(seq);;

let rec appartient (e: 'e) (ens: 'e ensemble) : bool = 
  match ens with
    | NIL -> false
    | Cons(a, subseq) -> a = e || appartient e subseq;;


let rec inclus (e1 : 'e ensemble)(e2 : 'e ensemble) : bool =
  match e1 with
    | NIL -> true
    | Cons(e, ens) -> appartient e e2 && inclus ens e2;;

let ajoute (e : 'e) (ens : 'e ensemble): 'e ensemble = Cons(e, ens);;

let rec supprime (e : 'e)(ens : 'e ensemble) : 'e ensemble =
  match ens with
    | NIL -> NIL
    | Cons(elem, sousEnsemble) -> if(e = elem) then sousEnsemble else Cons(elem, supprime e sousEnsemble);;
