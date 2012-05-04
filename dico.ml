(* Le dictionnaire efficace *)

(* Representation en forme d'arbre du dictionnaire *)
type dico = DNil | DNode of (dico * mot * dico);;

(*
  Specification:
  profil : present : mot -> dico -> bool
  Sémantique : present m d teste si le mot m est présent dans le dico d
  
  Realisation:
  Equation recursive:
  present (mot) DNil = false
  present (mot) DNode(dic1, smot, dic2) = si mot = smot alors true sinon 
  si mot > smot alors present mot dic1 sinon present mot dic2

  Termisaison:
  Soit la fonction mesure (dico) qui renvoie la hauteur de l'arbre
  on a mesure(DNode(dico, mot DNil)) = 1 + mesure ( dico)
  d'ou mesure(DNode(dico, mot DNil)) > mesure(dico)
  donc mesure décroissante minorée par 0 
  soit la fonction present termine
  
  Implantation:
*)

let rec present (mot :mot)(dico : dico) : bool =
  match dico with
    | DNil -> false
    | DNode(dic1, smot, dic2) when mot = smot -> true
    | DNode(dic1, smot, dic2) ->  if mot > smot then present mot dic1 else present mot dic2;;


(*
  Specification:
  profil : ajout_dico : mot -> dico -> dico
  Sémantique : ajout_dico m d ajoute le mot m dans le dico d
  
  Realisation:
  Equation recursive:
  present (mot) DNil = DNode(DNil, mot, DNil)
  present (mot) DNode(dic1, smot, dic2) = si mot > smot alors DNode(dic1, smot, ajout mot dic2)
  sinon DNode(ajout mot dic1, smot, dic2)

  Termisaison:
  Soit la fonction mesure (dico) qui renvoie la hauteur de l'arbre
  on a mesure(DNode(dico, mot DNil)) = 1 + mesure ( dico)
  d'ou mesure(DNode(dico, mot DNil)) > mesure(dico)
  donc mesure décroissante minorée par 0 
  soit la fonction present termine
  Implantation:
*)

let rec ajout_dico (mot:mot)(dico:dico) : dico = 
   match dico with
    | DNil -> DNode(DNil, mot, DNil)
    | DNode(dic1, smot, dic2) -> if mot > smot then DNode(dic1, smot, ajout mot dic2) else DNode(ajout mot dic1, smot, dic2);;

(* 
   Specification:
   profil : ajout : mot -> dico -> dico
   Sémantique : ajout m d ajoute le mot m au dico d si il n'est pas déjà présent ...
   
   Realisation:
   Implantation:
*)

let rec ajout (mot: mot)(dico:dico) : dico =
if present mot dico then
  dico
else
  ajout_dico mot dico;;
 

(* Petite fonction bonus de bonus pour visualiser le dictionnaire sous forme de liste *)
(*
  Specification :
  profil : afficherdictionnaire : dico -> mot list
  Sémantique : afficherdictionnaire d affiche le dictionnaire d sous forme de liste

  Realisation:
  Equation recursives :
  afficherdictionnaire DNil -> []
  afficherdictionnaire DNode(dic1, mot, dic2) -> (afficherdictionnaire dic1)@(mot::(afficherdictionnaire dic2))

  Terminaison: 
  Soit la fonction mesure (dico) qui renvoie la hauteur de l'arbre
  on a mesure(DNode(dico, mot DNil)) = 1 + mesure ( dico)
  d'ou mesure(DNode(dico, mot DNil)) > mesure(dico)
  donc mesure décroissante minorée par 0 
  soit la fonction present termine
*)
let rec afficherdictionnaire (dico:dico): mot list =
  match dico with
    | DNil -> []
    | DNode(dic1, mot, dic2) -> (afficherdictionnaire dic1)@(mot::(afficherdictionnaire dic2));;

(* Test d'ajout dans le dictionnaire *)
let nouveaudico = ajout (mot_of_string "quelle") DNil;;
let nouveaudico = ajout (mot_of_string "ministre") nouveaudico;;
let nouveaudico = ajout (mot_of_string "seche") nouveaudico;;
let nouveaudico = ajout (mot_of_string "sinistre") nouveaudico;;
let nouveaudico = ajout (mot_of_string "meche") nouveaudico;;

afficherdictionnaire nouveaudico;;
(* Nous avons maintenant un dictionnaire efficace *)
