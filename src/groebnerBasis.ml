(* Grobner basis computations for K[X]-module *)

(* Imports and abbreviations *)
    
open Utils
open Num.TaggedInfix
open Monalg

(* ------------------------------------------------------------------------- *)
(* Defining polynomial types                                                 *)
(* ------------------------------------------------------------------------- *)

module R = Monalg.IntField


module X = Monalg.Multinom(R)  (* the monomials over variables *)
module Y = Set.Make(R)         (* the set of private variables *)
module Z = Monalg.Multinom(R)  (* the monomials for ghost variables *)

module S = Monalg.MonAlg(X)(R)
module T = Monalg.MonAlg(Z)(R)

module P = Monalg.ProdAlg(S)(T)

(* ------------------------------------------------------------------------- *)
(* Reduce monomial cm by polynomial pol, returning replacement for cm.       *)
(* ------------------------------------------------------------------------- *)

let reduce1 (priv:Y.t) ((m, c) : X.t * R.t) ((p, q):P.t) =
  match (S.split p) with
    | None -> None
    | Some(((m2,c2), remainder)) -> 
      try
        let x, r = (X.( */ ) priv m m2, R.( ~!)  (R.( /! ) c c2)) in Some(P.( *! ) (S.form r x, T.form r x) (remainder, q))
        with X.DivFailure -> None

(* ------------------------------------------------------------------------- *)
(* Try this for all polynomials in a basis.                                  *)
(* ------------------------------------------------------------------------- *)
(*
exception ReduceFailure

let reduceb mp cm pols =
  try  List.find_map (reduce1 mp cm) pols
  with Not_found -> raise ReduceFailure
*)
(* ------------------------------------------------------------------------- *)
(* Reduction of a polynomial (always picking largest monomial possible).     *)
(* ------------------------------------------------------------------------- *)
(*
let rec reduce mp pols pol=
  match pol with
  | [] -> []
  | cm :: ptl ->
      try  reduce mp pols (mpoly_add (reduceb  mp cm pols) ptl)
      with ReduceFailure -> cm :: reduce mp pols ptl
*)
(* ------------------------------------------------------------------------- *)
(* Compute S-polynomial of two polynomials.                                  *)
(* ------------------------------------------------------------------------- *)
(*
let spoly mp pol1 pol2 :pol=
  match (pol1,pol2) with
  | ([], _ ) -> []
  | (_ , []) -> []

  | (m1 :: ptl1, m2 :: ptl2) ->
     let m = mlcm m1 m2 in
     mpoly_sub (mpoly_mmul mp (mdiv mp m m1) ptl1)
               (mpoly_mmul mp (mdiv mp m m2) ptl2)
*)
(* ------------------------------------------------------------------------- *)
(* Grobner basis algorithm for free multi-module                             *)
(* ------------------------------------------------------------------------- *)
(*
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
*)
(* ------------------------------------------------------------------------- *)
(* Overall function.                                                         *)
(* ------------------------------------------------------------------------- *)
(*
let groebner mp basis =
  grobner mp basis (distinctpairs basis)

let is_deduc mp basis (pol:pol) =
  List.is_empty (reduce mp basis pol)
*) 
