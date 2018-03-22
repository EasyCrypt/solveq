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

let mpoly_mmul vars mp cm (pol:pol) :pol= (map (mmul cm) pol);;

let mpoly_neg (pol:pol) :pol= (map (fun (c,m) -> (minus_num c,m)) pol);;

let mpoly_const (vars) c :pol=
  if c =/ Num.Int 0 then [] else [c,map (fun _ -> 0) vars];;

let rec mpoly_add (l1:pol) (l2:pol):pol =
  match (l1,l2) with
    ([],l2) -> l2
  | (l1,[]) -> l1
  | ((c1,m1)::o1,(c2,m2)::o2) ->
        if m1 = m2 then
          let c = c1+/c2 and rest = mpoly_add o1 o2 in
          if c = Num.Int 0 then rest else (c,m1)::rest
        else if morder_lt m2 m1 then (c1,m1)::(mpoly_add o1 l2)
        else (c2,m2)::(mpoly_add l1 o2);;

let mpoly_sub l1 l2 = mpoly_add l1 (mpoly_neg l2);;

(* ------------------------------------------------------------------------- *)
(* Reduce monomial cm by polynomial pol, returning replacement for cm.       *)
(* ------------------------------------------------------------------------- *)

let reduce1 vars mp cm pol =
  match pol with
    [] -> failwith "reduce1"
  | hm::cms -> let c,m = mdiv mp cm hm in mpoly_mmul vars mp (minus_num c,m) (cms);;

(* ------------------------------------------------------------------------- *)
(* Try this for all polynomials in a basis.                                  *)
(* ------------------------------------------------------------------------- *)

let reduceb vars mp cm pols = let res = tryfind (reduce1 vars mp cm) pols in
                                              res;;

(* ------------------------------------------------------------------------- *)
(* Reduction of a polynomial (always picking largest monomial possible).     *)
(* ------------------------------------------------------------------------- *)

let rec reduce vars mp pols pol=
  match pol with
    [] -> []
  | cm::ptl -> try reduce vars mp pols (mpoly_add (reduceb  vars mp cm pols) ptl)
               with Failure _ -> let pol = (reduce  vars mp pols ptl) in
                                 cm::pol;;

(* ------------------------------------------------------------------------- *)
(* Compute S-polynomial of two polynomials.                                  *)
(* ------------------------------------------------------------------------- *)

let spoly vars mp  pol1 pol2 :pol=
  match (pol1,pol2) with
    ([],_) -> []
  | (_,[]) -> []
  | (m1::ptl1,m2::ptl2) ->
     let m = mlcm m1 m2 in
        let res = mpoly_sub (mpoly_mmul vars mp  (mdiv mp m m1) ptl1)
                            (mpoly_mmul vars mp  (mdiv mp m m2) ptl2) in
        res;;

(* ------------------------------------------------------------------------- *)
(* Grobner basis algorithm for free multi-module                             *)
(* ------------------------------------------------------------------------- *)
  
  
let rec grobner vars mp basis pairs =
  match pairs with
    [] -> basis
  | (p1,p2)::opairs ->
        try
          let sp = reduce vars mp basis (spoly vars mp p1 p2) in
          if sp = [] then grobner vars mp basis opairs                              
          else 
            let newcps = map (fun p -> p,sp) basis in
              grobner vars mp (sp::basis) (opairs @ newcps)    
        with Failure _ -> grobner vars mp basis opairs;;
  
(* ------------------------------------------------------------------------- *)
(* Overall function.                                                         *)
(* ------------------------------------------------------------------------- *)

let groebner vars mp basis = grobner vars mp basis (distinctpairs basis);;

let is_deduc vars mp basis (pol:pol) =
  let red =  reduce vars mp basis pol in
                       red= [];;

  
(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

let x = 'x' and y = 'y' and z = 'z';;
let vars = [x;y;z];;

let mp = [1;1;0];; (* only z is fully known, and only g^x and g^y are known *)

let m1 = [(Num.Int 1,[1;0;0])];; (* x *)
let m2 = [(Num.Int 1,[0;1;0])];; (* y *)
let m3 = [(Num.Int 1,[0;0;1])];; (* z *)
let m4 = [(Num.Int 1,[1;1;0])];; (* xy *)
let m5 = [(Num.Int 1,[0;1;1])];; (* yz *)

let p1 = mpoly_add m4 m2;; (* xy+y*)

let gb = groebner vars mp ([p1;m4]);;
is_deduc vars mp gb m5;;
