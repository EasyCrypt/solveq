(* Grobner basis computations for K[X]-module *)

(* Imports and abbreviations *)
    
open Core
open Num.TaggedInfix
open Monalg

exception ReduceFailure

(* ------------------------------------------------------------------------- *)
(* Defining GB for a product algebra of polynoms.                            *)
(* We manipulate elements of the form (P,Q), where the second composante may *)
(* allow us to keep track of the computations                                *)
(* ------------------------------------------------------------------------- *)
    
module ProdGB(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t)(P : Monalg.ProductAlgebra with type ringA = S.t and type ringB = S.t) :
sig
  val groebner : Set.Make(Var).t -> P.t list -> P.t list
  val deduc : VarSet.t -> P.t list -> P.t -> S.t option
  val syz : VarSet.t -> P.t list -> S.t list

end = struct

let print = Format.printf 

  
  let is_zero ((p, q) : P.t) : bool =
    S.eq p S.zero

  (* ------------------------------------------------------------------------- *)
  (* Reduce monomial cm by polynomial pol, returning replacement for cm.       *)
  (* ------------------------------------------------------------------------- *)

  let reduce1 (priv : VarSet.t) ((m, c) : X.t * R.t) ((p, q) : P.t) =
    match (S.split p) with
    | None -> None
    | Some(((m2, c2), remainder)) -> 
      try
        let x, r = (X.( */ ) priv m m2, R.( ~!)  (R.( /! ) c c2)) in
        Some(P.( *! ) (S.form r x, S.form r x) (remainder, q))
      with X.DivFailure -> None

  (* ------------------------------------------------------------------------- *)
  (* Try this for all polynomials in a basis.                                  *)
  (* ------------------------------------------------------------------------- *)

  let reduceb mp cm pols =
    try  List.find_map (reduce1 mp cm) pols
    with Not_found -> raise ReduceFailure

  (* ------------------------------------------------------------------------- *)
  (* Reduction of a polynomial (always picking largest monomial possible).     *)
  (* ------------------------------------------------------------------------- *)

  let rec reduce mp pols ((p, q) : P.t)=
    match (S.split p) with
    | None -> Some(p,q)
    | Some(((m, c), remainder)) ->
      try
        let reducedpol = (P.(+!) (reduceb  mp (m, c) pols) (remainder,q)) in
        reduce mp pols reducedpol
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
      let mul1 = (S.form r1 x1, S.form r1 x1) and mul2 = (S.form r2 x2, S.form r2 x2) in
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
            if (is_zero sp) then 
              grobner priv basis opairs
            else
       	      let newcps = List.map (fun p -> p,sp) basis in
              grobner priv (sp::basis) (opairs @ newcps)
      with ReduceFailure | X.DivFailure -> grobner priv basis opairs

  let rec syzygie priv basis pairs syz=
    match pairs with
    | [] -> syz

    | (p1, p2) :: opairs ->
      try
	match (spoly priv p1 p2) with
	|None ->  syzygie priv basis opairs syz
	|Some(spol) ->
	  match (reduce priv basis spol) with
	  |None -> syzygie priv basis opairs syz
	  |Some(sp) ->
            if (is_zero sp) then 
              syzygie priv basis opairs ((P.pi2 sp)::syz)
            else
       	      let newcps = List.map (fun p -> p,sp) basis in
              syzygie priv (sp::basis) (opairs @ newcps) syz
      with ReduceFailure | X.DivFailure -> syzygie priv basis opairs syz
  (* ------------------------------------------------------------------------- *)
  (* Overall function.                                                         *)
  (* ------------------------------------------------------------------------- *)

  let groebner priv pols =
    grobner priv pols (List.product pols)

  let syz priv pols =
    syzygie priv pols (List.product pols) []

  (* deduc expects to get has input a groeber basis *)
  let deduc priv basis secret =
    match (reduce priv basis secret) with
    |None -> None
    |Some((p,q)) -> if (S.eq p S.zero) then Some(S.( ~!) q) else raise ReduceFailure
end




(* ------------------------------------------------------------------------- *)
(* Defining GB for basic polynoms.                                           *)
(* ------------------------------------------------------------------------- *)
module GB(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t) :
sig
  val groebner : Set.Make(Var).t -> S.t list -> S.t list
  val deduc : VarSet.t -> S.t list -> S.t -> S.t option
end = struct

  let is_zero (p:S.t) : bool =
    S.eq p S.zero

  (* ------------------------------------------------------------------------- *)
  (* Reduce monomial cm by polynomial pol, returning replacement for cm.       *)
  (* ------------------------------------------------------------------------- *)

  let reduce1 (priv : VarSet.t) ((m, c) : X.t * R.t) (p: S.t) =
    match (S.split p) with
    | None -> None
    | Some(((m2, c2), remainder)) -> 
      try
        let x, r = (X.( */ ) priv m m2, R.( ~!)  (R.( /! ) c c2)) in Some(S.( *! ) (S.form r x) (remainder))
      with X.DivFailure -> None

  (* ------------------------------------------------------------------------- *)
  (* Try this for all polynomials in a basis.                                  *)
  (* ------------------------------------------------------------------------- *)

  let reduceb mp cm pols =
    try  List.find_map (reduce1 mp cm) pols
    with Not_found -> raise ReduceFailure

  (* ------------------------------------------------------------------------- *)
  (* Reduction of a polynomial (always picking largest monomial possible).     *)
  (* ------------------------------------------------------------------------- *)

  let rec reduce mp pols (p: S.t)=
    match (S.split p) with
    | None -> Some(p)
    | Some(((m, c), remainder)) -> 
      try  reduce mp pols (S.(+!) (reduceb  mp (m, c) pols) remainder)
      with ReduceFailure -> 
      match (reduce mp pols remainder) with
      |None -> None
      |Some(p) -> Some(S.(+!) p  (S.form c m))

  (* ------------------------------------------------------------------------- *)
  (* Compute S-polynomial of two polynomials.                                  *)
  (* ------------------------------------------------------------------------- *)

  let spoly priv (p1: S.t) (p2: S.t) =
    match (S.split p1, S.split p2) with
    | (None, _ ) -> None
    | (_ , None) -> None
    | (Some(((m1, c1), r1)),Some(((m2, c2), r2))) ->
      let m = X.lcm m1 m2 and c = R.lcm c1 c2 in
      let x1, r1 = (X.( */ ) priv m m1, R.( ~!)  (R.( /! ) c c1)) in
      let x2, r2 = (X.( */ ) priv m m2, R.( ~!)  (R.( /! ) c c2)) in
      let mul1 = S.form r1 x1 and mul2 = S.form r2 x2 in
      Some(S.( -!) (S.( *! ) mul1 p1)
	     (S.( *! ) mul2 p2)
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
            if (is_zero sp) then 
              grobner priv basis opairs
            else
       	      let newcps = List.map (fun p -> p,sp) basis in
              grobner priv (sp::basis) (opairs @ newcps)
      with ReduceFailure | X.DivFailure -> grobner priv basis opairs


  (* ------------------------------------------------------------------------- *)
  (* Overall function.                                                         *)
  (* ------------------------------------------------------------------------- *)

  let groebner priv pols =
    grobner priv pols (List.product pols)
  
  (* deduc expects to get has input a groeber basis *)
  let deduc priv basis secret =
    match (reduce priv basis secret) with
    |None -> None
    |Some(p) -> Some(S.( ~!) p)
end
