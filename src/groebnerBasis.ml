(* Grobner basis computations for K[X]-module *)

(* Imports and abbreviations *)
    
open Utils
open Num.TaggedInfix

(* ------------------------------------------------------------------------- *)
(* Defining polynomial types                                                 *)
(* ------------------------------------------------------------------------- *)

type mon = Num.num * int list

type pol = mon list

type i_var_set = int list

(* ------------------------------------------------------------------------- *)
(* Operations on monomials.                                                  *)
(* ------------------------------------------------------------------------- *)

let mmul ((c1, m1) : mon) ((c2, m2) : mon) : mon =
  (c1 */ c2, List.map2 (+) m1 m2)

exception MDivFailure

let mdiv (pvars : i_var_set)=
  let index_sub n1 n2 = if n1 < n2 then raise MDivFailure else n1-n2 in

  fun ((c1, m1) : mon) ((c2 ,m2) : mon) ->

  let pows = List.map2 index_sub m1 m2 in
  let checker = List.combine pows pvars in

  if List.exists (fun (x, y) -> y = 1 && x > 0) checker then
    raise MDivFailure; 
  ((c1//c2, pows) : mon)

let mlcm ((_, m1) : mon) ((_, m2) : mon) : mon =
  (Num.Int 1, List.map2 max m1 m2)

(* ------------------------------------------------------------------------- *)
(* Monomial ordering.                                                        *)
(* ------------------------------------------------------------------------- *)

let morder_lt m1 m2 =
  let n1 = itlist (+) m1 0 and n2 = itlist (+) m2 0 in
  n1 < n2 || (n1 = n2 &&  lexord (>) m1 m2)

(* ------------------------------------------------------------------------- *)
(* Arithmetic on canonical multivariate polynomials.                         *)
(* ------------------------------------------------------------------------- *)

let mpoly_mmul mp cm (pol : pol) : pol =
  List.map (mmul cm) pol

let mpoly_neg (pol : pol) : pol =
  List.map (fun (c, m) -> (Num.minus_num c, m)) pol

let mpoly_const vars c : pol=
  if c =/ Num.Int 0 then [] else [c, List.map (fun _ -> 0) vars]

let rec mpoly_add (l1:pol) (l2:pol):pol =
  match (l1,l2) with
  | ([],l2) -> l2
  | (l1,[]) -> l1

  | ((c1, m1) :: o1, (c2, m2) :: o2) ->

    if m1 = m2 then
      let c = c1+/c2 and rest = mpoly_add o1 o2 in
      if c = Num.Int 0 then rest else (c,m1)::rest else
    
    if   morder_lt m2 m1
    then (c1, m1) :: mpoly_add o1 l2
    else (c2, m2) :: mpoly_add l1 o2

let mpoly_sub l1 l2 =
  mpoly_add l1 (mpoly_neg l2)

(* ------------------------------------------------------------------------- *)
(* Reduce monomial cm by polynomial pol, returning replacement for cm.       *)
(* ------------------------------------------------------------------------- *)

let reduce1 mp cm pol =
  match pol with
  | [] -> None

  | hm :: cms ->
      try
        let c, m = mdiv mp cm hm in
        Some (mpoly_mmul mp (Num.minus_num c,m) (cms))
      with MDivFailure -> None

(* ------------------------------------------------------------------------- *)
(* Try this for all polynomials in a basis.                                  *)
(* ------------------------------------------------------------------------- *)

exception ReduceFailure

let reduceb mp cm pols =
  try  List.find_map (reduce1 mp cm) pols
  with Not_found -> raise ReduceFailure

(* ------------------------------------------------------------------------- *)
(* Reduction of a polynomial (always picking largest monomial possible).     *)
(* ------------------------------------------------------------------------- *)

let rec reduce mp pols pol=
  match pol with
  | [] -> []
  | cm :: ptl ->
      try  reduce mp pols (mpoly_add (reduceb  mp cm pols) ptl)
      with ReduceFailure -> cm :: reduce mp pols ptl

(* ------------------------------------------------------------------------- *)
(* Compute S-polynomial of two polynomials.                                  *)
(* ------------------------------------------------------------------------- *)

let spoly mp pol1 pol2 :pol=
  match (pol1,pol2) with
  | ([], _ ) -> []
  | (_ , []) -> []

  | (m1 :: ptl1, m2 :: ptl2) ->
     let m = mlcm m1 m2 in
     mpoly_sub (mpoly_mmul mp (mdiv mp m m1) ptl1)
               (mpoly_mmul mp (mdiv mp m m2) ptl2)

(* ------------------------------------------------------------------------- *)
(* Grobner basis algorithm for free multi-module                             *)
(* ------------------------------------------------------------------------- *)

let rec grobner mp basis pairs =
  match pairs with
  | [] -> basis
  
  | (p1, p2) :: opairs ->
      try
        let sp = reduce mp basis (spoly mp p1 p2) in
        if List.is_empty sp then grobner mp basis opairs                                      else
          let newcps = List.map (fun p -> p,sp) basis in
            grobner mp (sp::basis) (opairs @ newcps)
      with ReduceFailure | MDivFailure -> grobner mp basis opairs

(* ------------------------------------------------------------------------- *)
(* Overall function.                                                         *)
(* ------------------------------------------------------------------------- *)

let groebner mp basis =
  grobner mp basis (distinctpairs basis)

let is_deduc mp basis (pol:pol) =
  List.is_empty (reduce mp basis pol)

