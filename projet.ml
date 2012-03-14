(* Projet INF121 : Ensemble, Multi-ensembles & Contrepets

           ENJOLRAS Clement - DERDAELE Jeremy


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
    | Cons(elem, sousEnsemble) when e = elem -> sousEnsemble
    | Cons(elem, sousEnsemble) -> Cons(elem, supprime e sousEnsemble);;

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
    | Cons(elem, sousEnsemble) when appartient elem e2 -> Cons(elem, intersection sousEnsemble e2)
    | Cons(elem, ens) -> intersection ens e2;;

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
    | Cons(elem, ens) when appartient elem e2 -> difference ens e2
    | Cons(elem, ens) -> Cons(elem, difference ens e2);;

(* FIXME: ComplexitÃ© minimale ? autre solution ? *)
(* On dÃ©finit une d'ajout d'ensembles pour faciliter la tache. *)
let rec ajoute_ensemble ( e1 : 'e ensemble) ( e2 : 'e ensemble) : 'e ensemble = 
  match e1 with
    | NIL -> e2
    | Cons(e, seq) -> Cons(e, ajoute_ensemble(seq)(e2));;

let differencesymetrique ( e1 : 'e ensemble) ( e2 : 'e ensemble) : 'e ensemble = 
ajoute_ensemble (difference e1 e2)(difference e2 e1);;

(* Partie 2 : Multi Elements et Multi Ensembles *)

(* Type Multi Element sous la forme
   du couple (element polymorphe, Occurence)
   on posera occurence différent de 0 car le multi
   ensemble perdrait son intérêt ... *)
type 'e multielt = 'e * int;;

type 'e multiens = VIDE | Add of 'e multielt * 'e multiens;;

#use "quickdev.ml"

let multivide = VIDE;;

let estvidemultiens (ens : 'e multiens) : bool = ens = VIDE;;

let rec cardinalmultiens (ens : 'e multiens) : int * int = 
  match ens with
    | VIDE -> (0,0)
    | Add((elem,count) , subens) -> let (a,b) = cardinalmultiens (subens)
      in (a+1, b+count);;

let rec occurencesmultiens (e : 'e) (ens : 'e multiens) : int =
    match ens with
      | VIDE -> 0
      | Add((el,count),_) when el = e -> count
      | Add(_,subseq) -> occurencesmultiens e subseq;;

let appartientmultiens (e :'e) ( ens : 'e multiens) : bool = occurencesmultiens e ens > 0;;

(* TODO: Question, les ensembles peuvent il etres inclus meme si 
deux memes elements on un nombre different d'occurence ??? *)

(*
profil: inclusmultiens: ’e multiens -> ’e multiens -> bool 
semantique: inclusmultiens (ens1) (ens2) renvoi true si le multi-ensemble ens1 est inclus dans le multi-ensemble ens2
exemples:

Realisation :
Equations récursives :
Terminaison :
implantation : *)

let rec inclusmultiens (e1 : 'e multiens)(e2 : 'e multiens) : bool = 
  match e1 with
    | VIDE -> true
    | Add((el, count), subseq) -> appartientmultiens el e2 && inclusmultiens subseq e2;;

(*
profil: ajoutemultiens: ’e mutielt  -> ’e multiens -> ’e multiens 

semantique: ajoute une ou plusieurs occurences d’élèment à un multi-ensemble.
exemples: (1) ajoutemultiens (1,2)(Add((1,1), Add((2,3), VIDE))) = Add ((1, 3), Add ((2, 3), VIDE))
          (2) ajoutemultiens (3,2)(Add((1,1), Add((2,3), VIDE))) = Add ((1, 1), Add ((2, 3), Add ((3, 2), VIDE)))

Realisation :
Equations récursives :
Terminaison :
implantation : *)
let rec ajoutemultiens ((e1,occ1):'e multielt) (ens:'e multiens): 'e multiens =
  match ens with
    | VIDE -> Add((e1,occ1), VIDE)
    | Add((e2,occ2), subseq) when e1 = e2 -> Add((e1,occ1+occ2), subseq)
    | Add(e,subseq) -> Add(e,ajoutemultiens(e1,occ1)(subseq));;

(*
profil: supprimemultiens: ’e multielt  -> ’e multiens -> ’e multiens 
semantique: supprime n occurences d’élèment d’un multi-ensemble. Si n est nul, supprime toutes les occurences de cet élément.
exemples: (1) supprimemultiens (3,2)(Add((1,1), Add((2,3), VIDE))) = Add((1,1), Add((2,3), VIDE))
          (2) supprimemultiens (2,2)(Add((1,1), Add((2,3), VIDE))) = Add((1,1), Add((2,1), VIDE))
          (3) supprimemultiens (2,0)(Add((1,1), Add((2,3), VIDE))) = Add ((1, 1), VIDE)

Realisation :
Equations récursives :
Terminaison :
implantation : *)
let rec supprimemultiens ((e1,occ1):'e multielt) (ens:'e multiens):'e multiens =
  match ens with
    | VIDE -> VIDE
    | Add((e2,occ2), subseq) when e1=e2 & ( (occ1 = 0) || (occ1>=occ2) ) -> subseq
    | Add((e2,occ2), subseq) when e1=e2 -> Add((e1,occ2 - occ1), subseq)
    | Add(e,subseq) -> Add(e,supprimemultiens(e1,occ1)(subseq));;

(*
profil: egauxmultiens: ’e multiens  -> ’e multiens -> bool 
semantique: egauxmultiens (ens1) (ens2) est vrai si les deux multi-ensembles sont égaux.
exemples: (1) egauxmultiens (Add((1,2),VIDE)) (Add((1,2),VIDE)) = true
          (2) egauxmultiens (Add((1,2),VIDE)) (Add((1,2),Add((2,1),VIDE))) = false

implantation : *)
let egauxmultiens (ens1:'e multiens) (ens2:'e multiens): bool =
  inclusmultiens ens1 ens2 && inclusmultiens ens2 ens1;;


(*
profil: unionmultiens: ’e multiens -> ’e multiens -> ’e multiens.
semantique: calcule l’union de deux multi-ensembles.
exemples: (1) unionmultiens (Add((1,2),VIDE)) (Add((1,2),Add((2,1),VIDE))) = Add ((1, 4), Add ((2, 1), VIDE))
          (2) unionmultiens (Add((1,2),VIDE)) (Add((3,1),VIDE)) =  Add ((3, 1), Add ((1, 2), VIDE))

Realisation :
Equations récursives :
Terminaison :
implantation : *)
let rec unionmultiens (ens1:'e multiens) (ens2:'e multiens):'e multiens =
  match ens1 with
    | VIDE -> ens2
    | Add(e, subseq) -> unionmultiens(subseq)(ajoutemultiens e ens2);;

(*
profil: intersectionmultiens: ’e multiens -> ’e multiens -> ’e multiens 
semantique: calcule l’intersection de deux multi-ensembles.
exemples:

Realisation :
Equations récursives :
Terminaison :
implantation : *)
let rec intersectionmutiens (ens1:'e multiens) (ens2:'e multiens):'e multiens =
  match ens1 with
    | VIDE -> VIDE;
    | Add((e1,occ1), subseq) when appartientmultiens (e1) (ens2) -> 

        let occ2 = occurencesmultiens (e1) (ens2) in	
      	  if (occ1 <= occ2) then
	    Add((e1,occ1), intersectionmutiens (subseq) (ens2))
	  else
	    Add((e1,occ2), intersectionmutiens (subseq) (ens2))

    | Add(_,subseq) ->  intersectionmutiens (subseq) (ens2);;

