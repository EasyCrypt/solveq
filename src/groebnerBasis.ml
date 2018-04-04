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

let reduce1 (priv : Y.t) ((m, c) : X.t * R.t) ((p, q) : P.t) =
  match (S.split p) with
    | None -> None
    | Some(((m2, c2), remainder)) -> 
      try
        let x, r = (X.( */ ) priv m m2, R.( ~!)  (R.( /! ) c c2)) in Some(P.( *! ) (S.form r x, T.form r x) (remainder, q))
        with X.DivFailure -> None

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

let rec reduce mp pols ((p, q) : P.t)=
  match (S.split p) with
  | None -> None
  | Some(((m, c), remainder)) -> 
      try  reduce mp pols (P.(+!) (reduceb  mp (m, c) pols) (remainder,q))
      with ReduceFailure -> 
        match (reduce mp pols (remainder, q)) with
            |None -> None
            |Some(pq) -> Some(P.(+!) pq (P.i1 (S.form c m)))

(* ------------------------------------------------------------------------- *)
(* Compute S-polynomial of two polynomials.                                  *)
(* ------------------------------------------------------------------------- *)

let spoly priv ((p1, q1) : P.t) ((p2, q2) : P.t) =
  match (S.split p1, S.split p2) with
  | (None, _ ) -> None
  | (_ , None) -> None
  | (Some(((m1, c1), r1)),Some(((m2, c2), r2))) ->
     let m = X.lcm m1 m2 and c = R.lcm c1 c2 in
     let x1, r1 = (X.( */ ) priv m m1, R.( ~!)  (R.( /! ) c c1)) in
     let x2, r2 = (X.( */ ) priv m m2, R.( ~!)  (R.( /! ) c c2)) in
     let mul1 = (S.form r1 x1, T.form r1 x1) and mul2 = (S.form r2 x2, T.form r2 x2) in
		Some(P.( -!) (P.( *! ) mul1 (p1, q1))
					 (P.( *! ) mul2 (p2, q2))
			)
(* ------------------------------------------------------------------------- *)
(* Grobner basis algorithm for free multi-module                             *)
(* ------------------------------------------------------------------------- *)

let rec grobner priv basis pairs =
  match pairs with
  | [] -> basis
  
  | (p1, p2) :: opairs ->
      try
		match (spoly priv p1 p2) with
			|None ->  grobner priv basis opairs
			|Some(spol) ->
	        match (reduce priv basis spol) with
				|None -> grobner priv basis opairs
				|Some(sp) ->
	        	  let newcps = List.map (fun p -> p,sp) basis in
    	        	grobner priv (sp::basis) (opairs @ newcps)
      with ReduceFailure | X.DivFailure -> grobner priv basis opairs

(* ------------------------------------------------------------------------- *)
(* Overall function.                                                         *)
(* ------------------------------------------------------------------------- *)

let groebner priv basis =
  grobner priv basis (distinctpairs basis)

let deduc priv basis secret =
	let basis = groebner priv basis in
  		(reduce priv basis secret)

