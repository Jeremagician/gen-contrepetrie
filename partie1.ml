(*
===========================================================
#  Projet INF121 : Ensemble, Multi-ensembles & Contrepets #
-----------------------------------------------------------
#          ENJOLRAS Clement - DERDAELE Jeremy             #
===========================================================


PARTIE I - Ensembles 
*)

(* Commencons par definir le type ensemble e

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

(*--------------
     Tests
---------------*)
estvide(NIL);; 
(* - : bool = true *)

estvide(Cons(42, NIL));; 
(* - : bool = false *)

(* Specification : 
   Profil : cardinal : 'e ensemble -> int
   Semantique : cardinal(e) renvoie le nombre d'élément (cardinal) 
   de l'ensemble e
   Exemples : (1) cardinal(NIL) = 0
              (2) cardinal(Cons(42, NIL)) = 1
              (3) cardinal(Cons(1, Cons(2,NIL))) = 2

   Realisation : 
   Equations Recursives :
    cardinal NIL = 0
    cardinal Cons(element, sousensemble) = 1 + cardinal sousensemble
    
   terminaison :
    Soit la fonction mesure ( ens ) = Nombre d'éléments de l'ensemble ens
    Profil mesure : 'e ensemble -> int
    On a mesure ( Cons(e,sousens) )  = Nombre d'éléments de (e,sousens)
                               > mesure ( sousens) = 1 + nombre d'éléments de sousens
    Ainsi mesure est une fonction décroissante et minorée par 0
    La fonction termine bien.
   implantation : *)

let rec cardinal(e: 'e ensemble) : int = match e with
  | NIL -> 0
  | Cons(_, seq) -> 1 + cardinal(seq);;

(*--------------
     Tests
---------------*)
cardinal(NIL);; 
(* int = 0 *)

cardinal(Cons(42, NIL));; 
(* int = 1 *)

cardinal(Cons(1, Cons(2, NIL)));; 
(* int = 2 *)

(* Specification : 
   Profil : appartient : 'e -> 'e ensemble -> bool
   Semantique : appartient(e)(ens) renvoie true si e appartient a l'ensemble ens
                et false sinon
   Exemples : (1) appartient (256)(Cons(42,Cons(255,NIL))) = false
              (2) appartient 42 (Cons(42, NIL)) = true

   Realisation : *
   
   Equations récursives :
      appartient (element)(NIL) = false
      appartient (element)(Cons(el, subseq)) = el = element || appartient element subseq
    
   Terminaison :
    Soit la fonction mesure ( ens ) = Nombre d'éléments de l'ensemble ens
    Profil mesure : 'e ensemble -> int
    On a mesure ( Cons(e,sousens) )  = Nombre d'éléments de (e,sousens)
                               > mesure ( sousens) = 1 + nombre d'éléments de sousens
    Ainsi mesure est une fonction décroissante et minorée par 0
    La fonction termine bien.
   implantation : *)

let rec appartient (e: 'e) (ens: 'e ensemble) : bool = 
  match ens with
    | NIL -> false
    | Cons(a, subseq) -> a = e || appartient e subseq;;

(*--------------
     Tests
---------------*)

appartient (256)(Cons(42, Cons(255, NIL)));; 
(* bool = false *)

appartient (42) (Cons(42, NIL));; 
(* bool = true *)

(* Specification : 
   Profil : inclus : 'e ensemble -> 'e ensemble -> bool
   Semantique : inclus e1 e2 renvoie true si e est inclus dans e2
   false sinon
   Exemples : (1) inclus(Cons(1,Cons(2,NIL)))(Cons(1,Cons(2,Cons(3,NIL)))) = true
              (2) inclus(Cons(42, NIL))(Cons(41,Cons(43,NIL))) = false

   Realisation :
   
   Equation recursive :
      inclus (NIL) (ensemble) = true
      inclus (Cons(element, sousensemble) = appartient element ensemble && inclus sousensemble ensemble
   Terminaison : 
        Soit la fonction mesure ( ens ) = Nombre d'éléments de l'ensemble ens
        Profil mesure : 'e ensemble -> int
        On a mesure ( Cons(e,sousens) )  = Nombre d'éléments de (e,sousens)
                               > mesure ( sousens) = 1 + nombre d'éléments de sousens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique mesure a l'ensemble passé en premier paramètre.
        La fonction termine bien.
   implantation : *)

let rec inclus (e1 : 'e ensemble)(e2 : 'e ensemble) : bool =
  match e1 with
    | NIL -> true
    | Cons(e, ens) -> appartient e e2 && inclus ens e2;;

(*--------------
     Tests
---------------*)

inclus(Cons(1,Cons(2,NIL)))(Cons(1,Cons(2,Cons(3,NIL))));; 
(* bool = true *)

inclus(Cons(42, NIL))(Cons(41,Cons(43,NIL)));; 
(* bool = false *)

(* Specification : 
   Profil : ajoute : 'e -> 'e ensemble -> 'e ensemble
   Semantique : ajoute 'e ('e ensemble) ajoute l'élément 'e à l'ensemble 'e ensemble
   Exemples : (1) ajoute 1 NIL = Cons(1,NIL)
              (2) ajoute 5 Cons(6,NIL) = Cons(5, Cons(6, NIL))

   Realisation : 
   implantation : *)

let ajoute (e : 'e) (ens : 'e ensemble): 'e ensemble = Cons(e, ens);;

(*--------------
     Tests
---------------*)

ajoute 1 NIL;;
(* - : int ensemble = Cons (1, NIL) *)

ajoute 5 (Cons(6,NIL));; 
(* - : int ensemble = Cons (5, Cons (6, NIL)) *)

(* Specification : 
   Profil : supprime : 'e -> 'e ensemble -> 'e ensemble
   Semantique : supprime e ens supprime l'élément e de l'ensemble ens si celui ci
   appartient 
   Exemples : (1) supprime 1 (Cons(2, NIL)) = Cons(2, NIL)
              (2) supprime 1 (Cons(2,Cons(1, Cons(0, NIL)))) = Cons(2, Cons(0,NIL))
   Realisation : 
   
   Equation recursives :
      supprime (element) (NIL) = NIL
      supprime (element)(Cons(elem, sousensemble)) = 
      sousensemble
      si element = elem
      
        Cons(elem, supprime element sousensemble)
      sinon
        
   Terminaison :
      Soit la fonction mesure ( ens ) = Nombre d'éléments de l'ensemble ens
      Profil mesure : 'e ensemble -> int
      On a mesure ( Cons(e,sousens) )  = Nombre d'éléments de (e,sousens)
                                       > mesure ( sousens) = 1 + nombre d'éléments de sousens
      Ainsi mesure est une fonction décroissante et minorée par 0.
      On applique mesure a l'ensemble passé en 2ème paramètre.
      La fonction termine bien.
      
   implantation : *)

let rec supprime (e : 'e)(ens : 'e ensemble) : 'e ensemble =
  match ens with
    | NIL -> NIL
    | Cons(elem, sousEnsemble) when e = elem -> sousEnsemble
    | Cons(elem, sousEnsemble) -> Cons(elem, supprime e sousEnsemble);;

(*--------------
     Tests
---------------*)
supprime 1 (Cons(2, NIL));; 
(* - : int ensemble = Cons (2, NIL) *)

supprime 1 (Cons(2,Cons(1, Cons(0, NIL))));; 
(* - : int ensemble = Cons (2, Cons (0, NIL)) *)

(* Specification : 
   Profil : egaux : 'e ensemble -> 'e ensemble -> bool
   Semantique : egaux(e1) (e2) renvoie true si e1 est égal à e2
                false sinon
   Exemples : (1) egaux (Cons(1, Cons(2, NIL))) (Cons(2, Cons(1, NIL))) = true
              (2) egaux (Cons(1,Cons(2, NIL))) (Cons(1, NIL)) = false

   Realisation : 
   implantation : *)
let egaux (e1: 'e ensemble)(e2 : 'e ensemble) : bool = inclus e1 e2 && inclus e2 e1;;

(*--------------
     Tests
---------------*)

egaux (Cons(1, Cons(2, NIL))) (Cons(2, Cons(1, NIL)));;
(* - : bool = true *)

egaux (Cons(1,Cons(2, NIL))) (Cons(1, NIL));;
(* - : bool = false *)

(* Specification : 
   Profil : intersection : 'e ensemble -> 'e ensemble -> 'e ensemble
   Semantique : intersection e1 e2 renvoie l'intersection des ensembles e1 et e2
   Exemples : (1) intersection (Cons(1, Cons(2, NIL))) (Cons(3, Cons(2, NIL))) = Cons(2,NIL)

   Realisation : 
   
   Equations récursives :
    intersection NIL ensemble = NIL
    intersection Cons(elem, sousensemble)) ensemble = 
      Cons(elem, intersection sousensemble ensemble)
      si appartient elem ensemble revoie true
      
      intersection ens e2
      sinon
      
   Terminaison :
        Soit la fonction mesure ( ens ) = Nombre d'éléments de l'ensemble ens
        Profil mesure : 'e ensemble -> int
        On a mesure ( Cons(e,sousens) )  = Nombre d'éléments de (e,sousens)
                                         > mesure ( sousens) = 1 + nombre d'éléments de sousens
        Ainsi mesure est une fonction décroissante et minorée par 0
        On applique mesure au premier paramètre de 'intersection'
        La fonction termine bien.
   implantation : *)
   
let rec intersection (e1 : 'e ensemble)(e2 : 'e ensemble) : 'e ensemble =
  match e1 with
    | NIL -> NIL
    | Cons(elem, sousEnsemble) when appartient elem e2 -> Cons(elem, intersection sousEnsemble e2)
    | Cons(elem, ens) -> intersection ens e2;;

(*--------------
     Tests
---------------*)

intersection (Cons(1, Cons(2, NIL))) (Cons(3, Cons(2, NIL)));;
(* - : int ensemble = Cons (2, NIL) *)


(* Specification : 
   Profil : difference : 'e ensemble -> 'e ensemble -> 'e ensemble
   Semantique : difference intersection e1 e2 renvoie la difference des ensembles e1 et e2
   Exemples : (1) difference (Cons(1, Cons(2, NIL))) (Cons(3, Cons(2, NIL))) = Cons(1,NIL)
              (2) difference (Cons(1,Cons(2, NIL)))(NIL)
   Realisation : 
   
   Equations récursives :
   
    difference NIL ensemble = NIL
    difference Cons(elem, sousensemble)) ensemble = 
      difference sousensemble ensemble
      si appartient elem ensemble revoie true
      
      Cons(elem, difference sousensemble ensemble)
      sinon

   
   Terminaison :
        Soit la fonction mesure ( ens ) = Nombre d'éléments de l'ensemble ens
        Profil mesure : 'e ensemble -> int
        On a mesure ( Cons(e,sousens) )  = Nombre d'éléments de (e,sousens)
                                         > mesure ( sousens) = 1 + nombre d'éléments de sousens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique mesure au premier paramètre de la fonction.
        La fonction termine bien.
        
   implantation : *)
let rec difference (e1 : 'e ensemble)(e2 : 'e ensemble) : 'e ensemble = 
  match e1 with
    | NIL -> NIL
    | Cons(elem, ens) when appartient elem e2 -> difference ens e2
    | Cons(elem, ens) -> Cons(elem, difference ens e2);;

(*--------------
     Tests
---------------*)

difference (Cons(1, Cons(2, NIL))) (Cons(3, Cons(2, NIL)));;
(* - : int ensemble = Cons (1, NIL) *)

difference (Cons(1,Cons(2, NIL)))(NIL);;
(* - : int ensemble = Cons (1, Cons (2, NIL)) *)

(* On définit une fonction d'ajout d'ensembles pour faciliter la tache. *)
(* Specification : 
   Profil : union : 'e ensemble -> 'e ensemble -> 'e ensemble
   Semantique : union e1 e2 renvoie l'union des ensembles e1 et e2
   Exemples : (1) union Cons(1, Cons(2, NIL)) Cons(3, Cons(2, NIL)) = Cons(1,Cons(2,Cons(3,NIL)))
              (2) union (Cons(1,Cons(2,Cons(3,NIL)))) (NIL) = Cons(1, Cons(2, Cons(3, NIL)))
   Realisation : 
   
   Equations récursives :
   
    union NIL ensemble = ensemble
    union Cons(elem, sousensemble)) ensemble = Cons(elem, union sousensemble ensemble)
    
   Terminaison :
      Soit la fonction mesure ( ens ) = Nombre d'éléments de l'ensemble ens
      Profil mesure : 'e ensemble -> int
      On a mesure ( Cons(e,sousens) )  = Nombre d'éléments de (e,sousens)
                                       > mesure ( sousens) = 1 + nombre d'éléments de sousens
      Ainsi mesure est une fonction décroissante et minorée par 0
      On applique mesure au premier paramètre de 'union'
      La fonction termine bien.
   implantation : *)

let rec union ( e1 : 'e ensemble) ( e2 : 'e ensemble) : 'e ensemble = 
  match e1 with
    | NIL -> e2
    | Cons(e, seq) when appartient e e2 -> union seq e2
    | Cons(e, seq) -> Cons(e, union(seq)(e2));;

(*--------------
     Tests
---------------*)
union (Cons(1,Cons(2,Cons(3,NIL)))) (NIL);;
(* - : int ensemble = Cons (1, Cons (2, Cons (3, NIL))) *)

union (Cons(1, Cons(2, NIL))) (Cons(3, Cons(2, NIL)));;
(* - : int ensemble = Cons (1, Cons (3, Cons (2, NIL))) *)


(* Specification : 
   Profil : differencesymetrique : 'e ensemble -> 'e ensemble -> 'e ensemble
   Semantique : differencesymetrique e1 e2 renvoie la différence symétrique des 
                ensembles e1 et e2
   Exemples : (1) differencesymetrique (Cons(1,Cons(2, NIL))) (Cons(2,Cons(3, NIL))) = Cons(1,Cons(3, NIL))
*)

let differencesymetrique ( e1 : 'e ensemble) ( e2 : 'e ensemble) : 'e ensemble = 
union (difference e1 e2)(difference e2 e1);;

(*--------------
     Tests
---------------*)

(* Partie 2 : Multi Elements et Multi Ensembles *)

(* Type Multi Element sous la forme
   du couple (element polymorphe, Occurence)
   on posera occurence différent de 0 car le multi
   ensemble perdrait alors son intérêt ... *)
type 'e multielt = 'e * int;;

type 'e multiens = VIDE | Add of 'e multielt * 'e multiens;;

#use "quickdev.ml"

let multivide = VIDE;;

(*
profil: estvidemultiens: ’e multiens -> bool 
semantique: estvidemultiens (ens1) est vrai si le multi-ensemble ens1 est vide.
exemples: (1) estvidemultiens (VIDE) = true
          (2) estvidemultiens (Add((1,1),VIDE) = false

implantation : *)
let estvidemultiens (ens : 'e multiens) : bool = ens = VIDE;;

(*--------------
     Tests
---------------*)
estvidemultiens(VIDE);;
(* - : bool = true *)

estvidemultiens(Add((1,1),VIDE));;
(* - : bool = false *)

(*
profil: cardinalmultiens:: ’e multiens -> int∗int 
semantique:  retourne le couple (nombre de multi-éléments, nombre total d’occurences d'éléments).
exemples: cardinalmultiens (Add((1,2), Add((2,3), VIDE))) = (2,5)
          cardinalmultiens (VIDE) = (0,0)


Realisation :
Equations récursives :
        cardinalmultiens (VIDE)  = (0,0)
        cardinalmultiens( Add((elem, count), subens) ) = (a+1, b+count) (avec (a,b) = cardinalmultiens (subens)
Terminaison :
        Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique la fonction mesure a l'ensemble passé en paramètre de la fonction cardinalmultiens
        La fonction termine bien.

implantation : *)
let rec cardinalmultiens (ens : 'e multiens) : int * int = 
  match ens with
    | VIDE -> (0,0)
    | Add((elem,count) , subens) -> let (a,b) = cardinalmultiens (subens)
      in (a+1, b+count);;

(*--------------
     Tests
---------------*)
cardinalmultiens (Add((1,2), Add((2,3), VIDE)));;
(* - : int * int = (2, 5) *)

cardinalmultiens (VIDE);;
(* - : int * int = (0, 0) *)

(*
profil: occurencesmultiens: ’e -> ’e multiens -> int 
semantique:  calcule le nombre d’occurences d’un élément dans un multi-ensemble.
exemples: occurencesmultiens (2) (Add((1,5), Add((2,2), VIDE))) = 2
          occurencesmultiens (3) (Add((1,5), Add((2,2), VIDE))) = 0

Realisation :

Equations récursives :
occurencemultiens (element) (VIDE) = 0
occurencemultiens (element) (Add((e1,count), subseq)) = count si e1 = element occurencemultiens (element) (subseq) sinon

Terminaison :
        Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0
        on applique la fonction mesure a l'ensemble passé en paramètre.
        La fonction termine bien.
implantation : *)

let rec occurencesmultiens (e : 'e) (ens : 'e multiens) : int =
    match ens with
      | VIDE -> 0
      | Add((el,count),_) when el = e -> count
      | Add(_,subseq) -> occurencesmultiens e subseq;;

(*--------------
     Tests
---------------*)
occurencesmultiens (2) (Add((1,5), Add((2,2), VIDE)));; 
(* - : int = 2 *)

occurencesmultiens (3) (Add((1,5), Add((2,2), VIDE)));;
(* - : int = 0 *)

(*
profil: appartientmultiens: ’e -> ’e multiens -> bool 
semantique:  teste l’appartenance d’un élément à un multi-ensemble.
exemples: occurencesmultiens (2) (Add((1,5), Add((2,2), VIDE))) = true
          occurencesmultiens (3) (Add((1,5), Add((2,2), VIDE))) = false

implantation : *)
let appartientmultiens (e :'e) ( ens : 'e multiens) : bool = occurencesmultiens e ens > 0;;

(*--------------
     Tests
---------------*)

appartientmultiens (2) (Add((1,5), Add((2,2), VIDE)));;
(* - : bool = true *)

appartientmultiens (3) (Add((1,5), Add((2,2), VIDE)));;
(* - : bool = false *)

(*
profil: inclusmultiens: ’e multiens -> ’e multiens -> bool 
semantique: inclusmultiens (ens1) (ens2) renvoi true si le multi-ensemble ens1 est inclus dans le multi-ensemble ens2
exemples:
            (1) inclusmultiens(Add((1,2), Add((2,5), VIDE)))(Add((0,2), Add((1,2), Add((2,2) , Add((3,6), VIDE))))) = true
            (2) inclusmultiens(VIDE)(Add((4,2), Add((2,4), VIDE))) = true
            (3) inclusmultiens(Add((1,2), VIDE))(VIDE) = false
Realisation :

Equations récursives :
inclusmultiens (VIDE) (enssemble2) = true
inclusmultiens (Add((el, count), subseq)) = appartientmultiens (enssemble1) (enssemble2)

Terminaison :
        Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique la fonction mesure au premier paramètre de la fonction.
        La fonction termine bien.
implantation : *)

let rec inclusmultiens (e1 : 'e multiens)(e2 : 'e multiens) : bool = 
  match e1 with
    | VIDE -> true
    | Add((el, count), subseq) -> appartientmultiens el e2 && inclusmultiens subseq e2;;

(*--------------
     Tests
---------------*)

inclusmultiens(Add((1,2), Add((2,5), VIDE)))(Add((0,2), Add((1,2), Add((2,2) , Add((3,6), VIDE)))));;
(* - : bool = true *)

inclusmultiens(VIDE)(Add((4,2), Add((2,4), VIDE)));;
(* - : bool = true *)

inclusmultiens(Add((1,2), VIDE))(VIDE);;
(* - : bool = false *)

(*
profil: ajoutemultiens: ’e mutielt  -> ’e multiens -> ’e multiens 

semantique: ajoute une ou plusieurs occurences d’élèment à un multi-ensemble.
exemples: (1) ajoutemultiens (1,2)(Add((1,1), Add((2,3), VIDE))) = Add ((1, 3), Add ((2, 3), VIDE))
          (2) ajoutemultiens (3,2)(Add((1,1), Add((2,3), VIDE))) = Add ((1, 1), Add ((2, 3), Add ((3, 2), VIDE)))

Realisation :
Equations récursives :
ajoutemultiens ((e1,occ1)) (VIDE) = Add((e1,occ1),VIDE) 
ajoutemultiens ((e1,occ1)) (Add((e2,occ2), subseq)) = Add((e1,occ1+occ2), subseq) si e1 = e2 Add(e,ajoutemultiens(e1,occ1)(subseq)) sinon

Terminaison :
        Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique la fonction mesure a l'ensemble passé en argument.
        La fonction termine bien.
implantation : *)
let rec ajoutemultiens ((e1,occ1):'e multielt) (ens:'e multiens): 'e multiens =
  match ens with
    | VIDE -> Add((e1,occ1), VIDE)
    | Add((e2,occ2), subseq) when e1 = e2 -> Add((e1,occ1+occ2), subseq)
    | Add(e,subseq) -> Add(e,ajoutemultiens(e1,occ1)(subseq));;

(*--------------
     Tests
---------------*)

ajoutemultiens (1,2)(Add((1,1), Add((2,3), VIDE)));; 
(* - : int multiens = Add ((1, 3), Add ((2, 3), VIDE)) *)

ajoutemultiens (3,2)(Add((1,1), Add((2,3), VIDE)));;
(* - : int multiens = Add ((1, 1), Add ((2, 3), Add ((3, 2), VIDE))) *)

(*
profil: supprimemultiens: ’e multielt  -> ’e multiens -> ’e multiens 
semantique: supprime n occurences d’élèment d’un multi-ensemble. Si n est nul, supprime toutes les occurences de cet élément.
exemples: (1) supprimemultiens (3,2)(Add((1,1), Add((2,3), VIDE))) = Add((1,1), Add((2,3), VIDE))
          (2) supprimemultiens (2,2)(Add((1,1), Add((2,3), VIDE))) = Add((1,1), Add((2,1), VIDE))
          (3) supprimemultiens (2,0)(Add((1,1), Add((2,3), VIDE))) = Add ((1, 1), VIDE)

Realisation :

Equations récursives :
supprimemultiens (element) (VIDE) = VIDE
supprimemultiens ((e1,occ1)) (Add((e2,occ2), subseq)) =  subseq si e1 = e2 et occ1 = 0 ou si e1 = e2 et occ1>=occ2
                                                         Add((e1,occ2 - occ1), subseq) si e1 = e2
                                                         Add((e1,occ1),supprimemultiens(e1,occ1)(subseq)) sinon

Terminaison :
        Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique mesure a l'ensemble passé en paramètre.
        La fonction termine bien.
implantation : *)
let rec supprimemultiens ((e1,occ1):'e multielt) (ens:'e multiens):'e multiens =
  match ens with
    | VIDE -> VIDE
    | Add((e2,occ2), subseq) when e1=e2 & ( (occ1 = 0) || (occ1>=occ2) ) -> subseq
    | Add((e2,occ2), subseq) when e1=e2 -> Add((e1,occ2 - occ1), subseq)
    | Add(e,subseq) -> Add(e,supprimemultiens(e1,occ1)(subseq));;

(*--------------
     Tests
---------------*)

supprimemultiens (3,2)(Add((1,1), Add((2,3), VIDE)));;
(* - : int multiens = Add ((1, 1), Add ((2, 3), VIDE)) *)

supprimemultiens (2,2)(Add((1,1), Add((2,3), VIDE)));;
(* - : int multiens = Add ((1, 1), Add ((2, 1), VIDE)) *)

supprimemultiens (2,0)(Add((1,1), Add((2,3), VIDE)));;
(* - : int multiens = Add ((1, 1), VIDE) *)

(*
profil: egauxmultiens: ’e multiens  -> ’e multiens -> bool 
semantique: egauxmultiens (ens1) (ens2) est vrai si les deux multi-ensembles sont égaux.
exemples: (1) egauxmultiens (Add((1,2),VIDE)) (Add((1,2),VIDE)) = true
          (2) egauxmultiens (Add((1,2),VIDE)) (Add((1,2),Add((2,1),VIDE))) = false

implantation : *)
let egauxmultiens (ens1:'e multiens) (ens2:'e multiens): bool =
  inclusmultiens ens1 ens2 && inclusmultiens ens2 ens1;;

(*--------------
     Tests
---------------*)
egauxmultiens (Add((1,2),VIDE)) (Add((1,2),VIDE));;
(* - : bool = true *)

egauxmultiens (Add((1,2),VIDE)) (Add((1,2),Add((2,1),VIDE)));;
(* - : bool = false *)

(*
profil: unionmultiens: ’e multiens -> ’e multiens -> ’e multiens.
semantique: calcule l’union de deux multi-ensembles.
exemples: (1) unionmultiens (Add((1,2),VIDE)) (Add((1,2),Add((2,1),VIDE))) = Add ((1, 4), Add ((2, 1), VIDE))
          (2) unionmultiens (Add((1,2),VIDE)) (Add((3,1),VIDE)) =  Add ((3, 1), Add ((1, 2), VIDE))

Realisation :

Equations récursives :
unionmultiens (VIDE) (enssemble2) = VIDE
unionmultiens (Add(e, subseq)) (enssemble2) = unionmultiens(subseq) (ajoutemultiens e ens2)

Terminaison :
        Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique mesure au premier paramètre de la fonction.
        La fonction termine bien.
implantation : *)
let rec unionmultiens (ens1:'e multiens) (ens2:'e multiens):'e multiens =
  match ens1 with
    | VIDE -> ens2
    | Add(e, subseq) -> unionmultiens(subseq)(ajoutemultiens e ens2);;

(*--------------
     Tests
---------------*)

unionmultiens (Add((1,2),VIDE)) (Add((1,2),Add((2,1),VIDE)));;
(* - : int multiens = Add ((1, 4), Add ((2, 1), VIDE)) *)

unionmultiens (Add((1,2),VIDE)) (Add((3,1),VIDE));;
(* - : int multiens = Add ((3, 1), Add ((1, 2), VIDE)) *)

(*
profil: intersectionmultiens: ’e multiens -> ’e multiens -> ’e multiens 
semantique: calcule l’intersection de deux multi-ensembles.
exemples: (1) intersectionmultiens (Add((1,2),Add((2,1),VIDE))) (Add((2,1),VIDE)) = (Add((2,1),VIDE))
          (2) intersectionmultiens (Add((1,3),Add((2,4),VIDE))) (Add((3,3),Add((2,2),VIDE))) = (Add((2,2),VIDE))

Realisation :
Equations récursives :
intersectionmultiens (VIDE) (enssemble2) = VIDE
intersection (Add((e1,occ1), subseq)) (ens2) = Add((e1,occ1), intersectionmultiens (subseq) (ens2))  si e1 appartien à ens2 et si l'occurence de l'element e1 est plus faible dans le premier ensemble que dans le second
                                               Add((e1,occ2), intersectionmutiens (subseq) (ens2)) si e1 appartient à ens2 et si l'occurence de l'element e1 est plus forte dans le premier ensemble que dans le second
                                               intersectionmultiens (subseq) (ens2) sinon

Terminaison :
  Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique la fonction mesure au premier paramètre.
        La fonction termine bien.
implantation : *)
let rec intersectionmultiens (ens1:'e multiens) (ens2:'e multiens):'e multiens =
  match ens1 with
    | VIDE -> VIDE;
    | Add((e1,occ1), subseq) when appartientmultiens (e1) (ens2) -> 

        let occ2 = occurencesmultiens (e1) (ens2) in	
      	  if (occ1 <= occ2) then
	    Add((e1,occ1), intersectionmultiens (subseq) (ens2))
	  else
	    Add((e1,occ2), intersectionmultiens (subseq) (ens2))

    | Add(_,subseq) ->  intersectionmultiens (subseq) (ens2);;

(*--------------
     Tests
---------------*)
intersectionmultiens (Add((1,2),Add((2,1),VIDE))) (Add((2,1),VIDE));;
(* - : int multiens = Add ((2, 1), VIDE) *)

intersectionmultiens (Add((1,3),Add((2,4),VIDE))) (Add((3,3),Add((2,2),VIDE)));;
(* - : int multiens = Add ((2, 2), VIDE) *)

(*
profil: differencemultiens: ’e multiens -> ’e multiens -> ’e multiens 
semantique: calcule la différence de deux multi-ensembles.
exemples:
  (1) differencemultiens (Add((1,4),Add((2,3),VIDE))) (Add((1,1),Add((2,3),VIDE))) = Add ((1, 3), VIDE)
Realisation :
Equations récursives :
differencemultiens VIDE ens2 = VIDE
differencemultiens (Add((e1,occ1), subseq)) (ens2) = differencemultiens (l'enssemble 1 - (e1,occurence de e1 dans e2)) (ens2 sans e1), si e1 appartien a ens2
                                                     Add ((e1,occ1), differencemultiens (subseq) (ens2) sinon
Terminaison :
implantation : *)

let rec differencemultiens (ens1:'e multiens) (ens2:'e multiens):'e multiens =
  match ens1 with
    | VIDE -> VIDE
    | Add((e1,occ1), subseq) when appartientmultiens (e1) (ens2) ->
        let occ2 = occurencesmultiens (e1) (ens2) in differencemultiens( supprimemultiens (e1,occ2) (ens1) ) (supprimemultiens(e1,0)(ens2)) 
    | Add(elem,subseq) -> Add(elem, differencemultiens (subseq) (ens2));;


(*--------------
     Tests
---------------*)
differencemultiens (Add((3,4),Add((2,5),VIDE))) (Add((1,1),Add((2,4),VIDE)));; 
(* int multiens = Add ((3, 4), Add ((2, 1), VIDE))*)

differencemultiens (Add((1,4),Add((2,3),VIDE))) (Add((1,1),Add((2,3),VIDE)));;
(* int multiens =  Add ((1, 3), VIDE) *)





let rec differencemultiens (ens1: 'e multiens) (ens2: 'e multiens) : 'e multiens =
if egauxmultiens (ens1) (ens2) then VIDE
else if inclusmultiens (ens2) (ens1) then match ens2 with 
    |Add((a,b),Add(d,g)) -> if appartientmultiens (a) (ens1) then
differencemultiens (supprimemultiens ((a,b)) (ens1)) (Add(d,g)) else differencemultiens
(ens1) (Add(d,g))
    |Add((a,b),VIDE) -> if appartientmultiens (a) (ens1) then
supprimemultiens ((a,b)) (ens1) else ens1
    |VIDE-> ens1 
else if inclusmultiens (ens1) (ens2) then VIDE 
else ens1;;
(*
profil: differencesymetriquemultiens: ’e multiens -> ’e multiens -> ’e multiens
semantique:  calcule la différence symétrique de deux multi-ensembles.
exemples: (1) 

Realisation :
implantation : *)
let differencesymetriquemultiens (ens1: 'e multiens) (ens2: 'e multiens) : 'e multiens =
  differencemultiens (unionmultiens(ens1) (ens2)) (intersectionmultiens (ens1) (ens2));;
(*--------------
     Tests
---------------*)


