(* Grobner basis computations for K[X]-module *)
#use "topfind";;
#require "num";;

(* Imports and abbreviations *)
    
open List;;
open Num;;

(* ------------------------------------------------------------------------- *)
(*  Utility functions                                                        *)
(* ------------------------------------------------------------------------- *)
  
let rec itlist f l b =
  match l with
    [] -> b
  | (h::t) -> f h (itlist f t b);;

let rec lexord ord l1 l2 =
  match (l1,l2) with
    (h1::t1,h2::t2) -> if ord h1 h2 then length t1 = length t2
                       else h1 = h2 && lexord ord t1 t2
  | _ -> false;;

let rec tryfind f l =
  match l with
      [] -> failwith "tryfind"
    | (h::t) -> try f h with Failure _ -> tryfind f t;;


let rec distinctpairs l =
  match l with
   x::t -> itlist (fun y a -> (x,y) :: a) t (distinctpairs t)
  | [] -> [];;


(* ------------------------------------------------------------------------- *)
(* Defining polynomial types                                                 *)
(* ------------------------------------------------------------------------- *)

type mon = Num.num * int list;;

type pol = mon list;;

type i_var_set = int list;;
  
(* ------------------------------------------------------------------------- *)
(* Operations on monomials.                                                  *)
(* ------------------------------------------------------------------------- *)

let mmul ((c1,m1):mon) ((c2,m2):mon) :mon  = (c1*/c2,map2 (+) m1 m2);;

let mdiv (pvars:i_var_set)=
  let index_sub n1 n2 = if n1 < n2 then failwith "mdiv" else n1-n2 in
  fun ((c1,m1):mon) ((c2,m2):mon) -> let pows = map2 index_sub m1 m2 in
                                     let checker = List.combine pows pvars in
                         if (List.fold_left (fun  a (x,y) -> (y=1 && x>0)|| a ) false checker) then failwith "mdiv" else
                         ((c1//c2,pows):mon);;

let mlcm ((_,m1):mon) ((_,m2):mon) :mon= (Num.Int 1,map2 max m1 m2);;
  
(* ------------------------------------------------------------------------- *)
(* Monomial ordering.                                                        *)
(* ------------------------------------------------------------------------- *)

let morder_lt m1 m2 =
  let n1 = itlist (+) m1 0 and n2 = itlist (+) m2 0 in
  n1 < n2 || n1 = n2 &&  lexord(>) m1 m2;;

(* ------------------------------------------------------------------------- *)
(* Arithmetic on canonical multivariate polynomials.                         *)
(* ------------------------------------------------------------------------- *)

let mpoly_mmul vars mp k_fQ cm (pol:pol) :pol= (map (mmul cm) pol);;

let mpoly_neg (pol:pol) :pol= (map (fun (c,m) -> (minus_num c,m)) pol);;

let mpoly_const (vars) c :pol=
  if c =/ Num.Int 0 then [] else [c,map (fun _ -> 0) vars];;

let rec mpoly_add (l1:pol) (l2:pol):pol =
  match (l1,l2) with
    ([],l2) -> l2
  | (l1,[]) -> l1
  | ((c1,m1)::o1,(c2,m2)::o2) ->
        if m1 = m2 then
          let c = c1+/c2 and rest = mpoly_add_aux o1 o2 in
          if c = Num.Int 0 then rest else (c,m1)::rest
        else if morder_lt m2 m1 then (c1,m1)::(mpoly_add_aux o1 l2)
        else (c2,m2)::(mpoly_add_aux l1 o2);;

let mpoly_sub l1 l2 = mpoly_add l1 (mpoly_neg l2);;

(* ------------------------------------------------------------------------- *)
(* Reduce monomial cm by polynomial pol, returning replacement for cm.       *)
(* ------------------------------------------------------------------------- *)

let reduce1 vars mp k_fQ cm (pol,pvars) =
  match pol with
    [] -> failwith "reduce1"
  | hm::cms -> let c,m = mdiv pvars cm hm in mpoly_mmul vars mp k_fQ (minus_num c,m) (cms);;

(* ------------------------------------------------------------------------- *)
(* Try this for all polynomials in a basis.                                  *)
(* ------------------------------------------------------------------------- *)

let reduceb vars mp k_fQ cm pols = let res = tryfind (reduce1 vars mp k_fQ cm) pols in
                                              res;;

(* ------------------------------------------------------------------------- *)
(* Reduction of a polynomial (always picking largest monomial possible).     *)
(* ------------------------------------------------------------------------- *)

let rec reduce vars mp k_fQ pols pol=
  match pol with
    [] -> []
  | cm::ptl -> try reduce vars mp k_fQ pols (mpoly_add (reduceb  vars mp k_fQ cm pols) ptl)
               with Failure _ -> let pol = (reduce  vars mp k_fQ pols ptl) in
                                 cm::pol;;

(* ------------------------------------------------------------------------- *)
(* Compute S-polynomial of two polynomials.                                  *)
(* ------------------------------------------------------------------------- *)

let spoly vars mp k_fQ  (pol1,pvar1) (pol2,pvar2) :pol=
  match (pol1,pol2) with
    ([],_) -> []
  | (_,[]) -> []
  | (m1::ptl1,m2::ptl2) ->
     let m = mlcm m1 m2 in
        let res = mpoly_sub (mpoly_mmul vars mp k_fQ  (mdiv pvar1 m m1) ptl1)
                            (mpoly_mmul vars mp k_fQ  (mdiv pvar2 m m2) ptl2) in
        res;;

(* ------------------------------------------------------------------------- *)
(* Grobner basis algorithm for free multi-module                             *)
(* ------------------------------------------------------------------------- *)
  
  
let rec grobner vars mp k_fQ basis pairs =
  match pairs with
    [] -> basis
  | (p1,p2)::opairs ->
        try
          let sp = reduce vars mp k_fQ basis (spoly vars mp k_fQ p1 p2) in
          if sp = [] then grobner vars mp k_fQ basis opairs                              
          else 
            let sp_pvars = map2 (fun x y -> x +y - x*y) (snd p1) (snd p2) in
            let newcps = map (fun p -> p,(sp,sp_pvars)) basis in
              grobner vars mp k_fQ ((sp,sp_pvars)::basis) (opairs @ newcps)    
        with Failure _ -> grobner vars mp k_fQ basis opairs;;
  
(* ------------------------------------------------------------------------- *)
(* Overall function.                                                         *)
(* ------------------------------------------------------------------------- *)

let groebner vars mp k_fQ basis = grobner vars mp k_fQ basis (distinctpairs basis);;

let is_deduc vars mp k_fQ basis (pol:pol) =
  let red =  reduce vars mp k_fQ basis pol in
                       red= [];;

groebmer [a,b,c]
