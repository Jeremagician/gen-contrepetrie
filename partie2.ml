(* PARTIE 2  *)
type 'a multiens = 'a multielt list;;

type 'a multielt = 'a * int;;

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

let cardinal (ens: 'e list): int = List.fold_left (+) 0 ens;;

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
(* type 'e ensemble = NIL | Cons of 'e * 'e ensemble;;|  WTF l'ennonce pourquoi ne pas utiliser une liste ???? *)

(* type dico = mot ensemble;; *)
type dico = mot list;;


(* "1 - Définissez mondico comme le dictionnaire contenant les mots de la contrepeterie donnée en exemple" *)
let mondico = [['q';'e';'l';'l';'e'];['m';'i';'n';'i';'s';'t';'r';'e'];['s';'e';'c';'h';'e']];;


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
ajout ['l';'a'] mondico;;
ajout ['p';'e';'r';'c';'e';'u';'s';'e'] mondico;;
ajout ['v';'i';'s';'s';'e';'u';'s';'e'] mondico;;

(* La perceuse visseuse *)

(* /!\ Il faut en trouver une autre mais y'as que des trucs trop salasse sur le net ... /!\ *)


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

 supprimeprefixecommun ['m';'o';'t';'e';'u';'r'] ['m';'o';'t';'u';'s'];;
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

suffixeegaux ['m';'o';'t';'e';'u';'r'] ['v';'o';'t';'e';'u';'r'];;
(* - : bool = true *)

suffixeegaux ['m';'o';'t';'e';'u';'r'] ['v';'o';'l';'e';'u';'r'];;
(* - : bool = false *)

suffixeegaux ['m';'o';'t';'e';'u';'r'] [];;
(* - : bool = false *)

suffixeegaux ['m';'o';'t';'e';'u';'r'] ['m';'o';'t'];;
(* - : bool = false *)


(* 3 - Specification :
Profil : deuxcontrepets : (mot ∗ mot) -> (mot ∗ mot) -> bool 
Semantique : détermine si deux couples de mots sont contrepets l’un de l’autre.
Exemples : 
Realisation :
implantation : 

/!\ Pas compris la fonction /!\ *)

(* 4 - Specification :
Profil : sontsimplecontrepeteries : phrase -> phrase -> bool. 
Semantique : détermine si deux couples de mots sont contrepets l’un de l’autre.rien compris
Exemples : 
Realisation :
implantation : 

/!\ Pas compris la fonction /!\ *)



(***************************************************
            Generateur de contrepets
****************************************************)



