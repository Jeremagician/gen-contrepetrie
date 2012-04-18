(* PARTIE 2  *)
(* /!\ CE N'EST PAS TOUT A FAIT CORRECT (gere pas le polymorphisme) *)
type multiens = (int * int) list;;

(* implantation 2 : *)
let rec cardinalmultiens (ens : multiens) : int * int =
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
let rec occurencesmultiens (e : int) (ens : multiens) : int =
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

(* implantation 2 : *)
let rec inclusmultiens (ens1: multiens)(ens2: multiens):bool =
  match ens1 with
    | [] -> true
    | (el,occ)::t -> appartientmultiens (el,occ) ens2 && inclusmultiens t ens2;;


(*--------------
     Tests
---------------*)

inclusmultiens  ([(1,2);(4,7)]) ([(1,2);(4,7);(2,2)]);;
(* - : bool = true *)

inclusmultiens([])([(4,2);(2,4)]);;
(* - : bool = true *)

inclusmultiens([(1,2);(2,2)])([]);;
(* - : bool = false *)

(* implantation 2 : *)
let rec ajoutemultiens ((e1,occ1): int * int)(ens: multiens): multiens =
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


(* implantation 2 : *)
let rec supprimemultiens ((e1,occ1):'e multielt) (ens: multiens): multiens =
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

(* implantation 2 : (rien n'as changé) *)
let egauxmultiens (ens1: multiens) (ens2: multiens): bool =
  inclusmultiens ens1 ens2 && inclusmultiens ens2 ens1;;

(*--------------
     Tests
---------------*)
egauxmultiens [(1,2)] [(1,2)];;
(* - : bool = true *)

egauxmultiens [(1,2)] [(1,2);(2,1)];;
(* - : bool = false *)

(* implantation 2 : *)
let rec unionmultiens (ens1: multiens) (ens2: multiens): multiens =
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

(* implantation 2 :*)
let rec intersectionmultiens (ens1: multiens) (ens2: multiens): multiens =
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


(* implantation 2 : *)
let rec differencemultiens (ens1: multiens) (ens2: multiens): multiens =
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



(***************************************************
            Reecritures avec fold
****************************************************)

(* implantation 2 : *)
let cardinal (ens: 'e list): int = List.fold_left (+) 0 ens;;

(*--------------
     Tests
---------------*)
cardinal [1;2;4];; 
(* - : int = 7 *)

cardinal [];;
(* - : int = 0 *)


(* implantation 3 : *)
let cardinalmultiens (ens: multiens): int*int =
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

let appartientmultiens ((el,occ) : int*int) ( ens : multiens) : bool = occurencesmultiens el ens >= occ;;



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
let inclusmultiens (ens1: multiens) (ens2: multiens): bool =
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
ajout ['l';'a'] dico;;
ajout ['p';'e';'r';'c';'e';'u';'s';'e'] dico;;
ajout ['v';'i';'s';'s';'e';'u';'s';'e'] dico;;

(* La perceuse visseuse *)

(* /!\ Il faut en trouver une autre mais y'as que des trucs trop salasse sur le net ... /!\ *)


(***************************************************
            Verificateur de contrepet
****************************************************)

(* 1 - Specification :
Profil : supprimeprefixecommun : mot -> mot -> (mot * mot)
Semantique : ajout un mot dans un dico.
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



