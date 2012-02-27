(* Projet INF121 : Ensemble, Multi-ensembles & Contrepets

           ENJOLRAS Clément - DERDAELE Jérémy


PARTIE I - Ensembles 
*)

(* Commencons par définir le type ensemble *)
type 'e ensemble = NIL | Cons of 'e * 'e ensemble;;

(* ainsi que l'ensemble vide *)
let ensemblevide = NIL;;

let estvide (e) : bool = e = ensemblevide;;

let rec cardinal(e) : int = match e with
  | NIL -> 0
  | Cons(_, seq) -> 1 + cardinal(seq);;

let rec appartient (e) (ens) = 
  match ens with
    | NIL -> false
    | Cons(a, subseq) -> a = e || appartient e subseq;;

let test = Cons(3, Cons(7, Cons(4, Cons(9, NIL))));;

appartient (7) (test);;

let ajoute (e) (ens) = Cons(e, ens);;

