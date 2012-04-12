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

(* implantation 2 : (rien ne change) *)
let appartientmultiens (e :'e) ( ens :multiens) : bool = occurencesmultiens e ens > 0;;



(* implantation 2 : *)
let rec inclusmultiens (ens1: multiens)(ens2: multiens):bool =
  match ens1 with
    | [] -> true
    | (el,_)::t -> appartientmultiens el ens2 && inclusmultiens t ens2;;


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
