open Core
open Types
open Monalg
    
module InvertRing(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t) : sig
 val compute_inv : var -> ring -> ring option
 val compute_inv_tuple : var list -> ring list ->  ring list option 
end = struct
  module C = Converter(R)(S)
  module Inv = Monalg_deduce.DeduceMonalg(R)(S)


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

