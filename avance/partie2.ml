(* PARTIE 2  *)

(* Definition des types *)
type 'a ensemble = 'a list;;
type 'a multielt = 'a * int;;
type 'a multiens = 'a multielt list;;

(* Specification : 
   Profil : appartient : 'e -> 'e ensemble -> bool
   Semantique : appartient(e)(ens) renvoie true si e appartient a l'ensemble ens
                et false sinon
   Exemples : (1) appartient (256)([255;2]) = false
              (2) appartient 42 ([15;42;24]) = true

   Realisation : *
   
   Equations récursives :
      appartient (element)(NIL) = false
      appartient (element)(h::t) = el = element || appartient element t
    
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
    | [] ->  false
    | h::t -> h = e || appartient e t;;


appartient (256)([255;2]);; (* - : bool = false *)

appartient 42 ([15;42;24]);; (* - : bool = true *)

(*
profil: cardinalmultiens:: ’e multiens -> int∗int 
semantique:  retourne le couple (nombre de multi-éléments, nombre total d’occurences d'éléments).
exemples: cardinalmultiens [(1,2);(2,3)] = (2,5)
          cardinalmultiens [] = (0,0)


Realisation :
Equations récursives :
        cardinalmultiens []  = (0,0)
        cardinalmultiens((elem, count)::t) = (a+1, b+count) (avec (a,b) = cardinalmultiens (subens)
Terminaison :
        Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique la fonction mesure a l'ensemble passé en paramètre de la fonction cardinalmultiens
        La fonction termine bien.

implantation : *)

(* implantation 2 : *)
let rec cardinalmultiens (ens : 'a multiens) : int * int =
  match ens with
    | [] -> (0,0)
    | (elem,count)::t -> let (a,b) = cardinalmultiens (t)
      in (a+1, b+count);;

(*--------------
Tests
---------------*)
cardinalmultiens [(1,2);(3,4);(5,2)];;
(* - : int * int = (3, 8) *)

cardinalmultiens [];;
(* - : int * int = (0, 0) *)


(* implantation 2 : *)

(*
profil: occurencesmultiens: ’e -> ’e multiens -> int 
semantique:  calcule le nombre d’occurences d’un élément dans un multi-ensemble.
exemples: occurencesmultiens (2) ([1,5];[2,2]) = 2
          occurencesmultiens (3) ([1,5];[2,2]) = 0

Realisation :

Equations récursives :
occurencemultiens (element) [] = 0
occurencemultiens (element) ((elem,count)::t) = count si e1 = element occurencemultiens (element) (subseq) sinon

Terminaison :
        Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0
        on applique la fonction mesure a l'ensemble passé en paramètre.
        La fonction termine bien.
implantation : *)
let rec occurencesmultiens (e : int) (ens : 'a multiens) : int =
    match ens with
      | [] -> 0
      | (el,count)::t -> if ( el = e ) then count else occurencesmultiens e t;;

(*--------------
     Tests
---------------*)
occurencesmultiens (2) ([(1,2);(3,4);(2,2)]);; 
(* - : int = 2 *)

occurencesmultiens (3) ([(5,1);(2,1)]);;
(* - : int = 0 *)

(*
profil: appartientmultiens: ’e -> ’e multiens -> bool 
semantique:  teste l’appartenance d’un élément à un multi-ensemble.
exemples: occurencesmultiens (2) ([(1,5);(2,2)]) = true
          occurencesmultiens (3) ([(1,5);(2,2)]) = false

implantation : *)
let appartientmultiens (e :'e) ( ens : 'e multiens) : bool = occurencesmultiens e ens > 0;;

(*--------------
     Tests
---------------*)

appartientmultiens (2) ([(1,5);(2,2)]);;
(* - : bool = true *)

appartientmultiens (3) ([(1,5);(2,2)]);;
(* - : bool = false *)

(*
profil: inclusmultiens: ’e multiens -> ’e multiens -> bool 
semantique: inclusmultiens (ens1) (ens2) renvoi true si le multi-ensemble ens1 est inclus dans le multi-ensemble ens2
exemples:
            (1) inclusmultiens([(1,2);(2,5)])([(0,2);(1,2);(2,2);(3,6)]) = true
            (2) inclusmultiens []([(4,2);(2,4)]) = true
            (3) inclusmultiens([(1,2)])[] = false
Realisation :

Equations récursives :
inclusmultiens [] (enssemble2) = true
inclusmultiens (el,count)::subseq) = appartientmultiens (enssemble1) (enssemble2)

Terminaison :
        Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique la fonction mesure au premier paramètre de la fonction.
        La fonction termine bien.
implantation : *)

let rec inclusmultiens (ens1: 'a multiens)(ens2: 'a multiens):bool =
  match ens1 with
    | [] -> true
    | (el,occ)::t -> appartientmultiens (el) ens2 && inclusmultiens t ens2;;


(*--------------
     Tests
---------------*)

inclusmultiens  ([(1,2);(4,7)]) ([(1,2);(4,7);(2,2)]);;
(* - : bool = true *)

inclusmultiens([])([(4,2);(2,4)]);;
(* - : bool = true *)

inclusmultiens([(1,2);(2,2)])([]);;
(* - : bool = false *)


(*
profil: ajoutemultiens: ’e mutielt  -> ’e multiens -> ’e multiens 

semantique: ajoute une ou plusieurs occurences d’élèment à un multi-ensemble.
exemples: (1) ajoutemultiens (1,2)([(1,1);(2,3)]) = [(1,3);(2,3)]
          (2) ajoutemultiens (3,2)([(1,1);(2,3)]) = [(1,1);(2,3);(3,2)]

Realisation :
Equations récursives :
ajoutemultiens ((e1,occ1)) [] =   [(e1,occ1)] 
ajoutemultiens ((e1,occ1)) ((e2,occ2)::subseq)) = (e1,occ1+occ2)::subseq) si e1 = e2 ajoutemultiens(e1,occ1)::(subseq)) sinon

Terminaison :
        Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique la fonction mesure a l'ensemble passé en argument.
        La fonction termine bien.
implantation : *)

let rec ajoutemultiens ((e1,occ1): 'a multielt)(ens: 'e multiens): 'e multiens =
  match ens with
    | [] -> [(e1,occ1)]
    | (e2,occ2)::t -> if (e1 = e2) then (e1,occ1+occ2)::t else (e2,occ2)::ajoutemultiens (e1,occ1) t;; 

(*--------------
     Tests
---------------*)

ajoutemultiens (1,2) [(1,1);(2,3)];;
(* - : multiens = [(1, 3); (2, 3)] *)

ajoutemultiens (3,2) [(1,1);(2,3)];;
(* - : multiens = [(1, 1); (2, 3); (3, 2)] *)



(*
profil: supprimemultiens: ’e multielt  -> ’e multiens -> ’e multiens 
semantique: supprime n occurences d’élèment d’un multi-ensemble. Si n est nul, supprime toutes les occurences de cet élément.
exemples: (1) supprimemultiens (3,2)([(1,1);(2,3)]) = [(1,1);(2,3)]
          (2) supprimemultiens (2,2)([(1,1);(2,3)]) = [(1,1);(2,1)]
          (3) supprimemultiens (2,0)([(1,1);(2,3)]) = [(1,1)]

Realisation :

Equations récursives :
supprimemultiens (element) [] = []
supprimemultiens ((e1,occ1)) (e2,occ2)::subseq) =  subseq si e1 = e2 et occ1 = 0 ou si e1 = e2 et occ1>=occ2
                                                         (e1,occ2 - occ1)::subseq si e1 = e2
                                                         (e1,occ1)::supprimemultiens(e1,occ1)(subseq) sinon

Terminaison :
        Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique mesure a l'ensemble passé en paramètre.
        La fonction termine bien.
implantation : *)

let rec supprimemultiens ((e1,occ1): 'a multielt) (ens: 'a multiens): 'a multiens =
  match ens with
    | [] -> []
    | (e2,occ2)::t when e1=e2 & ( (occ1 = 0) || (occ1>=occ2) ) -> t
    | (e2,occ2)::t when e1=e2                                  -> (e1,occ2 - occ1)::t
    | h::t                                                     -> h::supprimemultiens(e1,occ1)(t);;

(*--------------
     Tests
---------------*)

supprimemultiens (3,2) [(1,1);(2,3)];;
(* - :  multiens = [(1, 1); (2, 3)] *)

supprimemultiens (2,2)[(1,1);(2,3)];;
(* - : multiens = [(1, 1); (2, 1)] *)

supprimemultiens (2,0)[(1,1);(2,3)];;
(* - : multiens = [(1, 1)] *)


(*
profil: egauxmultiens: ’e multiens  -> ’e multiens -> bool 
semantique: egauxmultiens (ens1) (ens2) est vrai si les deux multi-ensembles sont égaux.
exemples: (1) egauxmultiens ([(1,2)])([(1,2)]) = true
          (2) egauxmultiens ([(1,2)]) ([(1,2);(2,1)]) = false

implantation : *)

let egauxmultiens (ens1: 'a multiens) (ens2: 'a multiens): bool =
  inclusmultiens ens1 ens2 && inclusmultiens ens2 ens1;;

(*--------------
     Tests
---------------*)
egauxmultiens [(1,2)] [(1,2)];;
(* - : bool = true *)

egauxmultiens [(1,2)] [(1,2);(2,1)];;
(* - : bool = false *)


(*
profil: unionmultiens: ’e multiens -> ’e multiens -> ’e multiens.
semantique: calcule l’union de deux multi-ensembles.
exemples: (1) unionmultiens ([(1,2)]) ([(1,2);(2,1)]) = [(1,4);(2,1)]
          (2) unionmultiens ([(1,2)]) ([(3,1)]) =  [(3,1);(1,2)]

Realisation :

Equations récursives :
unionmultiens [] (enssemble2) = []
unionmultiens (e, subseq)::(enssemble2) = unionmultiens(subseq) (ajoutemultiens e ens2)

Terminaison :
        Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
        Profil mesure : 'e multiens -> int
        On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                        > mesure ( subens) = 1 + nombre d'éléments de subens
        Ainsi mesure est une fonction décroissante et minorée par 0.
        On applique mesure au premier paramètre de la fonction.
        La fonction termine bien.
implantation : *)

let rec unionmultiens (ens1: 'a multiens) (ens2: 'a multiens): 'a multiens =
  match ens1 with
    | [] -> ens2
    | h::t -> unionmultiens(t)(ajoutemultiens h ens2);;

(*--------------
     Tests
---------------*)

unionmultiens [(1,2)] [(1,2);(2,1)];;
(* - : multiens = [(1, 4); (2, 1)] *)

unionmultiens [(1,2)] [(3,1)];;
(* - : multiens = [(3, 1); (1, 2)] *)


(*
profil: intersectionmultiens: ’e multiens -> ’e multiens -> ’e multiens 
semantique: calcule l’intersection de deux multi-ensembles.
exemples: (1) intersectionmultiens ([(1,2);(2,1)]) ([(2,1)]) = [(2,1)]
          (2) intersectionmultiens ([(1,3);(2,4)]) ([(3,3);(2,2)]) = [(2,2)]
Realisation :
Equations récursives :
intersectionmultiens [] (enssemble2) = []
intersection ((e1,occ1)::subseq)) (ens2) = (e1,occ1)::(intersectionmultiens (subseq) (ens2))  si e1 appartien à ens2 et si l'occurence de l'element e1 est plus faible dans le premier ensemble que dans le second
                                               (e1,occ2)::(intersectionmutiens (subseq) (ens2)) si e1 appartient à ens2 et si l'occurence de l'element e1 est plus forte dans le premier ensemble que dans le second
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

let rec intersectionmultiens (ens1: 'a multiens) (ens2: 'a multiens): 'a multiens =
  match ens1 with
    | [] -> [];
    | (e1,occ1)::t when appartientmultiens (e1) (ens2) ->

       let occ2 = occurencesmultiens (e1) (ens2) in
         if (occ1 <= occ2) then (e1,occ1)::intersectionmultiens (t) (ens2)
                           else (e1,occ2)::intersectionmultiens (t) (ens2)

    | _::t -> intersectionmultiens (t) (ens2);;

(*--------------
     Tests
---------------*)

intersectionmultiens [(1,2);(2,1)] [(2,1)];;
(* - : multiens = [(2, 1)] *)


intersectionmultiens [(1,3);(2,4)] [(3,3);(2,2)];;
(* - : multiens = [(2, 2)] *)



(*
profil: differencemultiens: ’e multiens -> ’e multiens -> ’e multiens 
semantique: calcule la différence de deux multi-ensembles.
exemples:
  (1) differencemultiens ([(1,4);(2,3)])([(1,1);(2,3)]) = [(1,3)]
Realisation :
Equations récursives :
differencemultiens [] ens2 = []
differencemultiens ((e1,occ)::subseq)) (ens2) = differencemultiens (l'enssemble 1 - (e1,occurence de e1 dans e2)) (ens2 sans e1), si e1 appartien a ens2
                                                     (e1,occ1)::(differencemultiens (subseq) (ens2)) sinon
Terminaison :
      Soit la fonction mesure ( multiens ) = Nombre de multi elements de l'ensemble (multiens)
      Profil mesure : 'e multiens -> int
      On a mesure ( Add((elem,count), subens))  = Nombre d'éléments de ((elem,count),subens)
                                      > mesure ( subens) = 1 + nombre d'éléments de subens
      Ainsi mesure est une fonction décroissante et minorée par 0.
      On applique la fonction mesure au premier paramètre.
      La fonction termine bien.
implantation : *)

let rec differencemultiens (ens1: 'a multiens) (ens2: 'a multiens): 'a multiens =
  match ens1 with
    | [] -> []
    | (e1,occ1)::t when appartientmultiens (e1) (ens2) ->
    
       let occ2 = occurencesmultiens (e1) (ens2) in
          differencemultiens( supprimemultiens (e1,occ2) (ens1) ) (supprimemultiens(e1,0)(ens2))

    | h::t -> h::differencemultiens (t) (ens2);;

(*--------------
     Tests
---------------*)
differencemultiens [(3,4);(2,5)] [(1,1);(2,4)];; 
(* - : multiens = [(3, 4); (2, 1)] *)

differencemultiens [(1,4);(2,3)] [(1,1);(2,3)];;
(* - : multiens = [(1, 3)] *)


(*
profil: differencesymetriquemultiens: ’e multiens -> ’e multiens -> ’e multiens
semantique:  calcule la différence symétrique de deux multi-ensembles.
exemples: (1) differencesymetriquemultiens ([(1,1);(2,1)])([(2,1);(3,1)]) = [(1,1);(3,1)]

Realisation :
implantation : *)
let differencesymetriquemultiens (ens1: 'e multiens) (ens2: 'e multiens) : 'e multiens =
  unionmultiens (differencemultiens(ens1)(ens2))(differencemultiens(ens2)(ens1));;
(*--------------
     Tests
---------------*)

differencesymetriquemultiens ([(1,1);(2,1)])([(2,1);(3,1)]);;
(* - : int multiens = [(3, 1); (1, 1)] *)


(***************************************************
            Reecritures avec fold
****************************************************)

(* Erreur *)

let cardinal (ens: 'e list): int = List.fold_left (fun a b -> a + 1) 0 ens;;

(*--------------
     Tests
---------------*)
cardinal [1;2;4];; 
(* - : int = 7 *)
cardinal [];;
(* - : int = 0 *)


(* implantation 3 : *)
let cardinalmultiens (ens: 'e multiens): int*int =
  List.fold_left (fun (e1,i1) (_,i2) -> (e1 + 1, i1+i2)) (0,0) ens;;

(*--------------
     Tests
---------------*)
cardinalmultiens [(1,2);(2,5);(4,1)];; 
(* - : int * int = (3, 8) *)

cardinalmultiens [];;
(* - : int * int = (0, 0) *)


(* necessaire pour inclus et inclusmultiens (cf : partie1) *)
let appartient (e: 'e) (ens: 'e list) : bool =
  List.fold_left (fun a b -> a || e = b) false ens;;

let appartientmultiens ((el,occ) : int*int) ( ens : 'e multiens) : bool = occurencesmultiens el ens >= occ;;



(* implantation 2 *)
let inclus (ens1: 'e list) (ens2: 'e list): bool =
  List.fold_left (fun a b -> a && (appartient b ens2)) true ens1;;

(*--------------
     Tests
---------------*)

inclus ['e';'b'] ['a';'b';'e'];;
(* - : bool = true *)

inclus ['e';'z'] ['a';'b';'e'];;
(* - : bool = false *)

inclus [] ['a';'b';'e'];;
(* - : bool = true *)

(* implantation 3 *)
let inclusmultiens (ens1: 'a multiens) (ens2: 'a multiens): bool =
  List.fold_left (fun a b -> a && (appartientmultiens b ens2)) true ens1;;

(*--------------
     Tests
---------------*)

inclusmultiens  ([(1,2);(4,7)]) ([(1,2);(4,7);(2,2)]);;
(* - : bool = true *)

inclusmultiens([])([(4,2);(2,4)]);;
(* - : bool = true *)

inclusmultiens([(1,2);(2,2)])([(1,1);(3,2)]);;
(* - : bool = false *)

(* implantation 3*)
let union (ens1: 'e list) (ens2: 'e list): 'e list =
  List.fold_left (fun a b -> if not(appartient b ens1) then a@[b] else a) ens1 ens2;;

(*--------------                                                                                                                                                                 
     Tests                                                                                                                                                                    
 ---------------*)
union([1;2;3;4]) ([3;4;5;6]);; (* - : int list = [1; 2; 3; 4; 5; 6] *)

union([1;2;3;4])([1;2;3;4]);; (* - : int list = [1; 2; 3; 4] *)


(* Implementation 3 *)

let unionmultiens (ens1 : 'e multiens)(ens2 : 'e multiens) : 'e multiens  =
  List.fold_left (fun a b -> ajoutemultiens b a) ens1 ens2;;

(*--------------                                                                                                                                                                 
     Tests                                                                                                                                                                    
 ---------------*)
unionmultiens [(1,3);(2,4)] [(1,2);(3,1)];; (* - : int multiens = [(1, 5); (2, 4); (3, 1)] *)

unionmultiens [] [(1,2)];; (* - : int multiens = [(1, 2)] *)


(* Implementation 3 *)

let intersection (ens1: 'e list) (ens2: 'e list): 'e list =
  List.fold_left (fun a b -> if appartient b ens1 then a@[b] else a) [] ens2;;

(* Tests *)

intersection ([1;2;3;4])([2;3;4;5]);; (* - : int list = [2; 3; 4] *)
intersection ([1;2;3])([4]);; (* - : int list = [] *)
intersection ([1;2;3])([]);; (* - : int list = [] *)

let intersectionmultiens (ens1 : 'e multiens)(ens2 : 'e multiens) : 'e multiens =
  List.fold_left (fun a b -> let (e2,occ2) = b in
                               if appartientmultiens e2 ens1 then 
                                 if occurencesmultiens e2 ens1 < occ2 then
                                   a@[(e2, occurencesmultiens e2 ens1)] 
                                 else
                                   a@[(e2,occ2)]
                               else
                                 a) [] ens2;;

(*--------------                                                                                                                                                                 
     Tests                                                                                                                                                                    
 ---------------*)

intersectionmultiens [(1,2);(2,3)] [(3,2);(2,5)];; (* - : int multiens = [(2, 3)] *)
intersectionmultiens [(1,2);(2,3)] [(4,2)];; (* - : int multiens = [] *)


(***************************************************
             Partie 4 : Dictionnaire
****************************************************)



type mot = char list;;
type dico = mot ensemble;; 
type phrase = mot list;;

(* Quelques fonction pour faciliter les futurss examples *)
let rec mot_of_string (s : string) : mot =
  let rec explode sub i =
    if i < 0 then sub else explode (s.[i] :: sub) (i-1)in
  explode [](String.length s - 1);;

let rec phrase_of_string(s : string) : phrase =
  let rec explode phrase mot i =
    if i < 0 then mot::phrase
    else
      if s.[i] = ' ' then
	explode (mot::phrase) [] (i-1)
      else
	explode phrase (s.[i]::mot) (i-1)
  in explode [] [] (String.length s-1);;


(* "1 - Définissez mondico comme le dictionnaire contenant les mots de la contrepeterie donnée en exemple" *)
let mondico = [mot_of_string "ministre"; mot_of_string "seche"];;

(* "2 - Programmez une fonction qui teste si un mot est dans un dictionnaire"

Specification :
Profil : present : mot -> dico -> bool
Semantique : present mot dico retourne true si mot est present dans dico. False sinon.
Exemples : (1) present ['m';'o';'t'] [['o';'u';'i'];['n';'o';'n']] = false
(2) present ['o';'u';'i'] [['o';'u';'i'];['n';'o';'n']] = true

implantation : *)
let present (mot: mot)(dico: dico):bool = List.fold_left ( fun a b -> a || b = mot ) false dico;;

(*--------------
     Tests
---------------*)
present ['m';'o';'t'] [['o';'u';'i'];['n';'o';'n']];;
(* - : bool = false *)

present ['o';'u';'i'] [['o';'u';'i'];['n';'o';'n']];;
(* - : bool = true *)

present ['s';'e';'c';'h';'e'] mondico;;
(* - : bool = true *)


(* "3 - Programmez une fonction d'ajout d'un mot dans un dictionnaire."

Specification :
Profil : ajout : mot -> dico -> dico
Semantique : ajout un mot dans un dico.
Exemples : (1) ajout ['m';'o';'t'] [['o';'u';'i'];['n';'o';'n']] = [['o';'u';'i'];['n';'o';'n'];['m';'o';'t']]
(2) ajout ['o';'u';'i'] [] = [['o';'u';'i']]

implantation : *)

let ajout (mot: mot) (dico: dico): dico = 
  if present mot dico then dico else mot::dico;;

(*--------------
     Tests
---------------*)

ajout ['m';'o';'t'] [['o';'u';'i'];['n';'o';'n']];;
(* - : dico = [['m'; 'o'; 't']; ['o'; 'u'; 'i']; ['n'; 'o'; 'n']] *)

ajout ['o';'u';'i'] [];;
(* - : dico = [['o'; 'u'; 'i']] *)

(* 4. Enrichissez ce dictionnaire afin de permettre deux exemples de vos propres contrepeteries. *)
let mondico = ajout (mot_of_string "la") mondico;;
let mondico = ajout (mot_of_string "perceuse") mondico;;
let mondico = ajout (mot_of_string "visseuse") mondico;;
let mondico = ajout (mot_of_string "sinistre") mondico;;
let mondico = ajout (mot_of_string "meche") mondico;;
let mondico = ajout (mot_of_string "verceuse") mondico;;
let mondico = ajout (mot_of_string "pisseuse") mondico;;

(* La perceuse visseuse *)

(***************************************************
            Verificateur de contrepet
****************************************************)

(* 1 - Specification :
Profil : supprimeprefixecommun : mot -> mot -> (mot * mot)
Semantique : supprime les prefixes commun entre les deux mots
Exemples : (1) supprimeprefixecommun ['m';'o';'t';'e';'u';'r'] ['m';'o';'t';'u';'s'] = (['e';'u';'r'], ['u';'s'])

Realisation :
Equations Recursives :
    supprimeprefiwecommun [] mot2 -> ([],mot2)
    supprimeprefixecommun h::t h2::t2 -> supprimeprefixecommun t t2 si h = h2
                                      -> (h::t, h2::t2) sinon

terminaison :
implantation : *)

(*/!\ Warnings a l'execution que je ne comprend pas *)
let rec supprimeprefixecommun (mot1: mot) (mot2: mot): (mot * mot) =
  match mot1 with
    | [] -> (mot1, mot2)
    | h::t when mot2 != [] -> let h2::t2 = mot2 in if (h = h2) then supprimeprefixecommun t t2 else (mot1,  mot2);;


(*--------------
     Tests
---------------*)

supprimeprefixecommun (mot_of_string "moteur")  (mot_of_string "motus");;
(* - : mot * mot = (['e'; 'u'; 'r'], ['u'; 's']) *)

 supprimeprefixecommun ['o';'u';'i'] ['n';'o';'n'];;
(* - : mot * mot = (['o'; 'u'; 'i'], ['n'; 'o'; 'n']) *)


(* 2 - Specification :
Profil : suffixeegaux : mot -> mot -> bool
Semantique : suffixeegaux mot1 mot2 renvoi true si les mots mot1 et mot2 ne differe que de la premier lettre, false sinon.
Exemples : (1) suffixeegaux ['m';'o';'t';'e';'u';'r'] ['v';'o';'t';'e';'u';'r'] = true
           (2) suffixeegaux ['m';'o';'t';'e';'u';'r'] ['v';'o';'l';'e';'u';'r'] = false
           (3) suffixeegaux ['m';'o';'t';'e';'u';'r'] ['m';'o';'t'] = false

Realisation :
implantation : *)

let suffixeegaux (mot1: mot) (mot2: mot): bool =
 if ((List.length mot1 != List.length mot2) || mot1 = [] || mot2 = []) then 
   false 
 else
   let _::suffixe1 = mot1 and _::suffixe2 = mot2 in List.fold_left2 (fun a b c -> a && (b = c) ) true suffixe1 suffixe2;;
   

(*--------------
     Tests
---------------*)

suffixeegaux (mot_of_string "moteur") (mot_of_string "voteur");;
(* - : bool = true *)

suffixeegaux (mot_of_string "moteur") (mot_of_string "voteur");;
(* - : bool = false *)

suffixeegaux (mot_of_string "moteur") [];;
(* - : bool = false *)

suffixeegaux (mot_of_string "moteur") (mot_of_string "mot");;
(* - : bool = false *)

(* 3 - Specification :
Profil : deuxcontrepets : (mot * mot) -> (mot * mot) -> bool 
Semantique : détermine si deux couples de mots sont contrepets l’un de l’autre.
Exemples : 
Realisation :
implantation : 
*)

let deuxcontrepets ((mot11,mot12) : (mot*mot))((mot21,mot22) : (mot*mot)): bool = 
  let (mot1,mot2) = supprimeprefixecommun (mot11)(mot21) and (mot3,mot4) = supprimeprefixecommun (mot12)(mot22) 
  in suffixeegaux(mot1)(mot2) && suffixeegaux(mot3)(mot4);;
deuxcontrepets(mot_of_string "ministre", mot_of_string "seche")(mot_of_string "sinistre", mot_of_string "meche");;
(* - : bool = true *)
deuxcontrepets(mot_of_string "salut", mot_of_string "patrick")(mot_of_string "saput", mot_of_string "latrick");;
(* - : bool = true *)



(* Specification
Profil : estcontrepetrievalide : mot list -> mot list -> bool
Semantique : estcontrepetrievalide phrase1 phrase2 retourne true si les listes sont de la forme [mot;mot] et que les mots sont deux contrepets

implantation :
*)

let estcontrepetrievalide (phr1 : phrase)( phr2 : phrase) : bool =
  if List.length phr1 <> 2 || List.length phr2 <> 2 then
      false
    else
      let h1::(t1::[]) = phr1 and h2::(t2::[]) = phr2 in deuxcontrepets(h1,t1)(h2,t2);;


(* 4 - Specification :
Profil : sontsimplecontrepeteries : phrase -> phrase -> bool. 
Semantique : détermine si deux couples de mots sont contrepets l’un de l’autre.rien compris
Exemples : sontsimplescontrepeteries (phrase_of_string "quelle ministre tres seche")(phrase_of_string "quelle sinistre tres meche") = true
           sontsimplescontrepeteries phrase_of_string "quelle ministre trop seche")(phrase_of_string "quelle sinistre tres meche") = false
Realisation :

   profil : iterate -> mot list -> mot list -> mot list
   semantique : iterate phrase1 refe supprime l'intersection de phrase1 et refe
   
   Equation rrecursive : iterate [] refe -> []
                         iterate h::t refe -> si h appartient a refe alors iterate t refe
                                              sinon h::(iterate t refe)
   Terminaison : soit mesure (phrase1) le nombre de mot de mot de phrase1;
                 mesure (mot::phrase1) = 1 + mesure (phrase1) donc
                 mesure est decroissante donc iterate termine.

implantation : 
*)

let sontsimplecontrepeteries (phr1: phrase) (phr2: phrase): bool=
  let rec iterate (phr : phrase) (refe : phrase) : phrase =
    match phr with
      | [] -> []
      | h::t -> if appartient h refe then iterate t refe else h::(iterate t refe)
  in 
    estcontrepetrievalide (iterate phr1 phr2)(iterate phr2 phr1);;


(* test *)

sontsimplecontrepeteries(phrase_of_string "quelle ministre tres seche")(phrase_of_string "quelle sinistre tres meche");; (* - : bool = true *)
sontsimplecontrepeteries (phrase_of_string "quelle ministre trop seche")(phrase_of_string "quelle sinistre tres meche");; (* - : bool = false *)


(***************************************************
            Generateur de contrepets
****************************************************)

(* 
profil : decompose :   mot -> (mot * char * mot) ensemble
semantique : decompose m retourne l'ensemble des decompositions du mot m.
exemple decompose ['m';'i';'n';'i';'s';'t';'r';'e'] = - : (mot * char * mot) ensemble =
 [([], 'm', ['i'; 'n'; 'i'; 's'; 't'; 'r'; 'e']);
 (['m'], 'i', ['n'; 'i'; 's'; 't'; 'r'; 'e']);
 (['m'; 'i'], 'n', ['i'; 's'; 't'; 'r'; 'e']);
 (['m'; 'i'; 'n'], 'i', ['s'; 't'; 'r'; 'e']);
 (['m'; 'i'; 'n'; 'i'], 's', ['t'; 'r'; 'e']);
 (['m'; 'i'; 'n'; 'i'; 's'], 't', ['r'; 'e']);
 (['m'; 'i'; 'n'; 'i'; 's'; 't'], 'r', ['e']);
 (['m'; 'i'; 'n'; 'i'; 's'; 't'; 'r'], 'e', [])]

profil : build : mot*mot -> mot*char*mot ensemble
semantique : construit la liste des decompositions du mot passé en argument de decompose
exemple : cf decompose

equation recursive
   build m1 [] -> ens
   build m1 h::t -> (m1, h, t)::(build (m1@[h], t) (ens))

terminaison
   soit mesure(mot2) = nombre de lettre de mot2.
   mesure('char'::mot2) = mesure(mot2) +1
   d'ou mesure('char'::mot2) > mesure(mot2)
   d'ou mesure est décroissante, minorée par 0
   build termine !

*)
let decompose (m : mot) : (mot * char * mot) ensemble =
  let rec build ((m1, m2): (mot *mot))(ens : (mot * char * mot) list) = 
    match m2 with
      | [] -> ens
      | h::t -> (m1, h, t)::(build (m1@[h], t) (ens))
  in build([], m) [];;
    

(* test *)
decompose (mot_of_string "ministre");;
(* - : (mot * char * mot) ensemble =
[([], 'm', ['i'; 'n'; 'i'; 's'; 't'; 'r'; 'e']);
 (['m'], 'i', ['n'; 'i'; 's'; 't'; 'r'; 'e']);
 (['m'; 'i'], 'n', ['i'; 's'; 't'; 'r'; 'e']);
 (['m'; 'i'; 'n'], 'i', ['s'; 't'; 'r'; 'e']);
 (['m'; 'i'; 'n'; 'i'], 's', ['t'; 'r'; 'e']);
 (['m'; 'i'; 'n'; 'i'; 's'], 't', ['r'; 'e']);
 (['m'; 'i'; 'n'; 'i'; 's'; 't'], 'r', ['e']);
 (['m'; 'i'; 'n'; 'i'; 's'; 't'; 'r'], 'e', [])] *)

decompose [];; (* - : (mot * char * mot) ensemble = [] *)


(* Specification
   profil : echange : (mot * char * mot) -> (mot * char * mot) -> (mot * mot)
   exemple : echange ([],'m',['i';'n';'i';'s';'t';'r';'e']) ([],'s',['e';'c';'h';'e'])
*)

let echange ((debut1,l1,fin1) : (mot * char * mot)) ((debut2,l2,fin2) : (mot * char * mot)) : (mot*mot) =
  (debut1@(l2::fin1), debut2@(l1::fin2));;

(* test *)
echange ([],'m',['i';'n';'i';'s';'t';'r';'e']) ([],'s',['e';'c';'h';'e']);;
(* - : mot * mot = (['s'; 'i'; 'n'; 'i'; 's'; 't'; 'r'; 'e'], ['m'; 'e'; 'c'; 'h'; 'e']) *)

(* Specification
   profil : contrepetrie : phrase -> dico -> phrase ensemble
*)

(* ATTENTION : Lisez la fonction progressivement, en suivant les commentaires !!! *)
(*


*)
(*
let contrepetrie (phrase : phrase)(dico : dico) : phrase ensemble =
  let rec generate (phr : phrase) (deb: phrase) (ens : phrase ensemble): phrase ensemble =
    (* On commence par parcourir la phrase afin de trouver un mot appartenant au dictionnaire *)
    match phr with
      | [] -> ens
      | h::t -> if present h dico then
	    (* ce mot est dans le dictionnaire ! on parcours la fin de la phrase pour trouver un second mot *)
	  let rec subgenerate (subphr : phrase)(subdeb : phrase)(ens1 : phrase ensemble) =
	      match subphr with
		| [] -> ens1
		| sh::st -> if present sh dico then
		    (* Ce nouveau mot est present aussi dans le dictionnaire, on va pouvoir generer des contrepetrie avec ces mots *)
		      (* On va parcourir l'emsemble des decompositions du premier mot et essayer des contrepetries *)
		    let rec parcourirdecompmot1 decompmot1 = 
		      match decompmot1 with
			| [] -> subgenerate st (subdeb@[sh]) ens1
			(* Pour chaque décomposition, nous allons parcourir la decomposition du second mot *)
			| dh::dt -> let rec parcourirdecompmot2 decompmot2 =
			    match decompmot2 with
			      | [] -> parcourirdecompmot1 dt
			      | dh2::dt2 -> let (mot1, mot2) = echange(dh)(dh2) in (* ça foire ici !!! *)
				  (* on verifie si les nouveau mot sont bien dans le dictionnaire et que la phrase est une simple contrepetrie *)
				  if present mot1 dico & present mot2 dico & sontsimplecontrepeteries phrase (deb@[mot1]@subdeb@[mot2]@st) then
				      (deb@[mot1]@subdeb@[mot2]@st)::ens1
				 else
				    parcourirdecompmot2 dt2
			  in parcourirdecompmot2 (decompose sh)
		    in parcourirdecompmot1 (decompose h)
	          (* Ce mot n'est pas une contrepétrie.. on continue en sauvegardant notre progression dans le fin de la phrase *)
		  else subgenerate (st)(subdeb@[sh]) ens1
	  in subgenerate t [] ens
	(* Ce mot n'est pas dans le dictionnaire, on continue d'iterer en sauvegardant notre progression *)
	else generate t (deb@[h]) ens
in generate phrase [] [];;
*)

let rec finirdecomp decomp

let rec parcourirdecomp (phrase:phrase)(dico:dico)((debut, milieu, fin) : (phrase*phrase*phrase)) (decomp1)(decomp2): phrase ensemble =
  match decomp1 with
      | [] -> []
      | h::t -> (finirdecomp )::parcourirdecomp phrase dico ((debut, milieu, fin)) (t)(decomp2)

let rec finirphrase (phrase:phrase)(dico:dico)((deb, fin):(phrase*phrase)) (mot1:mot)(debut:phrase): phrase ensemble =
  match fin with
    |[] -> []
    | h::t -> if prensent h dico then
	(* On retourne la nouvelle phrase *)
	(parcourirdecomp phrase dico (debut, deb@[h], t) mot1 h)@finirphrase phrase dico(deb@[h], t) mot1
      else
	finirphrase phrase dico (deb@[h], t) mot1;;

let rec parcourirphrase (phrase:phrase)(dico:dico)((debut, fin):(phrase*phrase)) : phrase ensemble =
  match fin with
    | [] -> []
    | h::t -> if present h dico then
	(finirphrase phrase dico ([],t) h debut)@(parcourirphrase phrase dico (debut@[h],t))
      else
	parcourirphrase phrase dico(debut@[h],t);;

let contrepetrie (phrase : phrase)(dico : dico) : phrase ensemble =
  parcourirphrase phrase dico ([],phrase);;

contrepetrie (phrase_of_string "quelle ministre seche") mondico;;
(*#########################################################*)
(*#                                                       #*)
(*#                         BONUS                         #*)
(*#                                                       #*)
(*#########################################################*)
