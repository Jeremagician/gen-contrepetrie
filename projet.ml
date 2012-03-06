(* Projet INF121 : Ensemble, Multi-ensembles & Contrepets

           ENJOLRAS Clement - DERDAELE Jeremy


PARTIE I - Ensembles 
*)

(* Commencons par definir le type ensemble 

Il s'agit ici d'un type dit "polymorphe"
qui ne prend pas de type particulier,
Mais l'ensembles des elements doivent 
êtres du même type
*)
type 'e ensemble = NIL | Cons of 'e * 'e ensemble;;


(* On définit l'enemblevide égal à NIL *)
let ensemblevide = NIL;;

(* Temporary for quick dev *)
#use "quickdev.ml";;

(* Specification : 
   Profil : estvide : 'e ensemble -> bool
   Semantique : estvide(e) renvoie true si e est un
   ensemble vide, et false sinon
   Exemples : (1) estvide(NIL) = true
              (2) estvide(Cons(42, NIL)) = false

   Realisation : 
   implantation : *)
let estvide (e: 'e ensemble) : bool = e = ensemblevide;;


(* Specification : 
   Profil : cardinal : 'e ensemble -> int
   Semantique : cardinal(e) renvoie le nombre d'élément (cardinal) 
   de l'ensemble e
   Exemples : (1) cardinal(NIL) = 0
              (2) cardinal(Cons(42, NIL)) = 1
              (3) cardinal(Cons(1, Cons(2,NIL))) = 2

   Realisation : 
   implantation : *)
let rec cardinal(e: 'e ensemble) : int = match e with
  | NIL -> 0
  | Cons(_, seq) -> 1 + cardinal(seq);;


(* Specification : 
   Profil : appartient : 'e -> 'e ensemble -> bool
   Semantique : appartient(e)(ens) renvoie true si e appartient a l'ensemble ens
                et false sinon
   Exemples : (1) appartient (256)(Cons(42,Cons(255,NIL))) = false
              (2) appartient 42 (Cons(42, NIL)) = true

   Realisation : *
   
   Equation récursives :

   Terminaison

   implantation : *)

let rec appartient (e: 'e) (ens: 'e ensemble) : bool = 
  match ens with
    | NIL -> false
    | Cons(a, subseq) -> a = e || appartient e subseq;;


(* Specification : 
   Profil : inclus : 'e ensemble -> 'e ensemble -> bool
   Semantique : inclus e1 e2 renvoie true si e est inclus dans e2
   false sinon
   Exemples : (1) inclus(Cons(1,Cons(2,NIL)))(Cons(1,Cons(2,Cons(3,NIL)))) = true
              (2) inclus(Cons(42, NIL))(Cons(41,Cons(43,NIL))) = false

   Realisation :
   
   Equation recursive :
   Terminaison : 
   implantation : *)

let rec inclus (e1 : 'e ensemble)(e2 : 'e ensemble) : bool =
  match e1 with
    | NIL -> true
    | Cons(e, ens) -> appartient e e2 && inclus ens e2;;

(* Specification : 
   Profil : ajoute : 'e -> 'e ensemble -> 'e ensemble
   Semantique : ajoute 'e ('e ensemble) ajoute l'élément 'e à l'ensemble 'e ensemble
   Exemples : (1) ajoute 1 NIL = Cons(1,NIL)
              (2) ajoute 5 Cons(6,NIL) = Cons(5, Cons(6, NIL))

   Realisation : 
   implantation : *)
let ajoute (e : 'e) (ens : 'e ensemble): 'e ensemble = Cons(e, ens);;

(* Specification : 
   Profil : supprime : 'e -> 'e ensemble -> 'e ensemble
   Semantique : supprime e ens supprime l'élément e de l'ensemble ens si celui ci
   appartient 
   Exemples : (1) supprime 1 Cons(2, NIL) = Cons(2, NIL)
              (2) supprime 1 Cons(2,Cons(1, Cons(0, NIL))) = Cons(2, Cons(0,NIL))
   Realisation : 
   
   Equation recursives :
   Terminaison :

   implantation : *)
let rec supprime (e : 'e)(ens : 'e ensemble) : 'e ensemble =
  match ens with
    | NIL -> NIL
    | Cons(elem, sousEnsemble) -> if(e = elem) then sousEnsemble else Cons(elem, supprime e sousEnsemble);;

(* Specification : 
   Profil : egaux : 'e ensemble -> 'e ensemble -> bool
   Semantique : egaux(e1) (e2) renvoie true si e1 est égal à e2
                false sinon
   Exemples : (1) egaux Cons(1, Cons(2, NIL)) Cons(2, Cons(1, NIL)) = true
              (2) egaux Cons(1,Cons(2, NIL)) Cons(1, NIL) = false

   Realisation : 
   implantation : *)
let egaux (e1: 'e ensemble)(e2 : 'e ensemble) : bool = inclus e1 e2 && inclus e2 e1;;

(* Specification : 
   Profil : intersection : 'e ensemble -> 'e ensemble -> 'e ensemble
   Semantique : intersection e1 e2 renvoie l'intersection des ensembles e1 et e2
   Exemples : (1) intersection Cons(1, Cons(2, NIL)) Cons(3, Cons(2, NIL)) = Cons(2,NIL)

   Realisation : 
   
   Equations récursives :
   Terminaison :
   implantation : *)
let rec intersection (e1 : 'e ensemble)(e2 : 'e ensemble) : 'e ensemble =
  match e1 with
    | NIL -> NIL
    | Cons(elem, ens) -> if appartient elem e2 then Cons(elem, intersection ens e2) else intersection ens e2;;

(* Specification : 
   Profil : difference : 'e ensemble -> 'e ensemble -> 'e ensemble
   Semantique : difference intersection e1 e2 renvoie la difference des ensembles e1 et e2
   Exemples : (1) intersection Cons(1, Cons(2, NIL)) Cons(3, Cons(2, NIL)) = Cons(1,NIL)

   Realisation : 
   
   Equations récursives :
   Terminaison :
   implantation : *)
let rec difference (e1 : 'e ensemble)(e2 : 'e ensemble) : 'e ensemble = 
  match e1 with
    | NIL -> NIL
    | Cons(elem, ens) -> if appartient elem e2 then difference ens e2 else Cons(elem, difference ens e2);;

(* FIXME: ComplexitÃ© minimale ? autre solution ? *)
(* On dÃ©finit une d'ajout d'ensembles pour faciliter la tache. *)
let rec ajoute_ensemble ( e1 : 'e ensemble) ( e2 : 'e ensemble) : 'e ensemble = 
  match e1 with
    | NIL -> e2
    | Cons(e, seq) -> Cons(e, ajoute_ensemble(seq)(e2));;

let differencesymetrique ( e1 : 'e ensemble) ( e2 : 'e ensemble) : 'e ensemble = 
ajoute_ensemble (difference e1 e2)(difference e2 e1);;

(* Partie 2 : Multi Elements et Multi Ensembles *)
