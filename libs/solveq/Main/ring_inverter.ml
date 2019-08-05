open Core
open Types
open Monalg
open GroebnerBasis

module VarSet = Set.Make(Var) (* an empty set *)
    

module InvertMonalg(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t) : sig
  val euclidian_div : var -> S.t -> S.t * S.t
  val inverter_tuple : var list -> S.t list -> S.t list
end = struct
  module GB = GB(R)(S)
      
  let split_pol (v:var) p1 =
    let rec split_pol_acc (v:var) p1 p2 =
      (* on input p1,p2, splits p1 into p,p' where p' does not depend on v,
         and returns (p,p'+p2) *)
      match (S.split p1) with
      | None -> (S.zero,p2)
      | Some(((x,r),p)) ->
        let (g1,g2) = split_pol_acc v p p2 and m = S.form r x in
        if X.getpow x v != 0 then
          (S.(+!) m g1,g2)
        else
          (g1,S.(+!) m g2) in
    split_pol_acc v p1 S.zero


  let rec div_and_indep (v:var) p =
    (* tries to produce the division of p by v, and should provide a polynom
       which does not contain v *)
    match (S.split p) with
    | None -> S.zero
    | Some(((x,r),p)) ->
      if (X.getpow x v) != 1 then
        raise NoInv
      else
        S.(+!) (S.form r (X.( */ ) (VarSet.empty) x (X.ofvar v))) (div_and_indep v p)


  (* Given a variable v and a pol p, compute p1 and p2 such that v=v*p1 + p2 *)
  let euclidian_div (v:var) (p:S.t) =
      let (p1,p2)= split_pol v p in (* we split p in p1+p2, where p2 does not contain v *)
      let p1 = div_and_indep v p1 in (* we divide p1 by v, and obtain a polynom independant from v *)
      (* Here p = v*p1+p2 *)
      (p1,p2)

  module C = Converter(R)(S)
  (* given a function v1,..,vn -> p1,...,pn, computes f:x1,...,xn -> e1,..,en such that
     f(p1,...,pn) = v1,...,vn *)
  let inverter_tuple (vars:var list) (pols:S.t list) : S.t list=
    if List.length vars != List.length pols then raise NoInv;
    let counter = ref (0) in
    let ps = List.fold_left (fun acc poly ->
        counter := !counter+1;
        let fresh_var =  S.form R.unit (X.ofvar (Var.make_fresh (Var.of_int (!counter)) ))  in
        (S.(-!) poly fresh_var )::acc
      ) [] pols in 
    
    let basis =  GB.groebner VarSet.empty ps in
    let inverters = List.map  (
        fun e->  match (GB.deduc VarSet.empty basis (S.form R.unit (X.ofvar (e)))) with
          |None -> raise NoInv
          |Some(q) -> q
            
        ) vars
    in  

    (* must check in deduc if the reduced form contains only -t elements *)
    let private_vars = (VarSet.of_list vars) in (* we thus define every variables as private, not know to the adversary *)
    List.map (fun poly ->
          let module M = Map.Make(X) in
          let varset = C.varset poly in
          if VarSet.is_empty (VarSet.inter varset private_vars) then (S.(~!) poly)
              else raise NoInv ) inverters

end

module InvertRing(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t) : sig
 val compute_inv : var -> ring -> ring option
 val compute_inv_tuple : var list -> ring list ->  ring list option 
end = struct
  module C = Converter(R)(S)
  module Inv = InvertMonalg(R)(S)


  (* Given a ring element r depending on v, try to compute the inverse of
     v -> r(v) *)
  let compute_inv (v:var) (r:ring) =
    try
      let p = C.ring_to_monalg r in (* we put p in normal form, inside a monalg *)
      let (p1,p2) = Inv.euclidian_div v p in
      let p1 = C.monalg_to_ring p1 and p2 = C.monalg_to_ring p2 in
      match p1 with
      |UnitR -> Some(AddR(VarR v, OppR(p2)))
      |_ -> Some(MultR(AddR(VarR v, OppR(p2)), InvR(p1))) (* we return a fraction, might be necessary to check that inv not null *)      
    with NoInv ->

    try
      let p,q = frac_to_ring r in
      if p = VarR v then
        Some( MultR(VarR v,q))
      else
        None
  (*let p,q = C.ring_to_monalg p,C.ring_to_monalg q in
      let (p1,p2) = Inv.euclidian_div v p in
      let (q1,q2) = Inv.euclidian_div v q in
      let p1 = C.monalg_to_ring p1 and p2 = C.monalg_to_ring p2 and  q1 = C.monalg_to_ring p1 and q2 = C.monalg_to_ring q2
      in
      Format.fprintf "%a %a %a %a" pp_ring p1 pp_ring p2 pp_
      if p1 = ZeroR && q2 = ZeroR && q1 = UnitR then  (* r = p2/x *)
  Some( MultR( VarR v, InvR(q2)))
    else None *)

      
      with NoInv -> None

  (* Given a list of ring element rs = r1,...,rn depending on a list of variables 
      vs = v1,...,vn , tries to compute the inverse of
     (v1,...,vn) -> (r1,...,rn) 
     The inverse is given as a function from VarR 1,...,VarR n -> exprs
     We test the membership of a sub algebra, following a classical GB encoding.
     For now, all variables in rs should appear in vs.
     cf exemples_inv.ml 
  *)
  let compute_inv_tuple (vs:var list) (rs:ring list) : ring list option=
    if List.length vs != List.length rs then raise NoInv;
    try
      let vars = vs and pols =  List.map C.ring_to_monalg rs in
      
      let inverters = Inv.inverter_tuple vars pols in

      Some( List.map C.monalg_to_ring inverters)
    with NoInv -> None
  end

