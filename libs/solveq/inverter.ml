open Core
open Types
open Monalg
open GroebnerBasis

module Empty = Set.Make(V) (* an empty set *)
    

module InvertMonalg(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t) =
struct
  module C = Converter(R)(S)
  module GB = GroebnerBasis(R)(S)
      
  let split_pol (v:pvar) p1 =
    let rec split_pol_acc (v:pvar) p1 p2 =
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


  let rec div_and_indep (v:pvar) p =
    (* tries to produce the division of p by v, and should provide a polynom
       which does not contain v *)
    match (S.split p) with
    | None -> S.zero
    | Some(((x,r),p)) ->
      if (X.getpow x v) != 1 then
        raise NoInv
      else
        S.(+!) (S.form r (X.( */ ) (Empty.empty) x (X.ofvar v))) (div_and_indep v p)


  (* Given a ring element r depending on v, try to compute the inverse of
     v -> r(v) *)
  let compute_inv (v:var) (r:ring) =
    try
      let p = C.ring_to_monalg r in (* we put p in normal form, inside a monalg *)
      let sv = pvar_of_var v in
      let (p1,p2)= split_pol sv p in (* we split p in p1+p2, where p2 does not contain v *)
      let p1 = div_and_indep sv p1 in (* we divide p1 by v, and obtain a polynom independant from v *)
      (* Here p = v*p1+p2 *)
      let p1 = C.monalg_to_ring p1 and p2 = C.monalg_to_ring p2 in
      match p1 with
      |UnitR -> Some(AddR(VarR v, OppR(p2)))
      |_ -> Some(MultR(AddR(VarR v, OppR(p2)), InvR(p1))) (* we return a fraction, might be necessary to check that inv not null *)
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
      let private_vars = (Y.of_list (List.map pvar_of_var vs)) in (* we first define every variables as private, not know to the adversary *)
      let counter = ref (0) in
      let mapping = Array.make (List.length rs) ZeroR in
      let ps = List.fold_left (fun acc elem ->
          counter := !counter+1;
          let fresh_var =  S.form R.unit (X.ofvar (pvar_of_var (!counter)  ~pref:"f" ))  in
          let poly = C.ring_to_monalg elem in
          (S.(-!) poly fresh_var )::acc
        ) [] rs in 

      let basis =  GB.groebner Y.empty ps in
      let inverters = List.map  (
          fun e->  match (GB.deduc Y.empty basis (S.form R.unit (X.ofvar (pvar_of_var e)))) with
            |None -> raise NoInv
            |Some(q) ->
              let module M = Map.Make(X) in
              let module Se = Set.Make(V) in
              let varset = C.varset q in
              if Se.is_empty (Se.inter varset private_vars) then (C.monalg_to_ring (S.(~!) q))
              else raise NoInv
        ) vs
      in  (* must check in deduc if the reduced form contains only -t elements *)
      Some(inverters)
    with NoInv -> None
end

module RingInv =  InvertMonalg(R)(S)

let compute_inv_ring_tuple = RingInv.compute_inv_tuple
let compute_inv_ring = RingInv.compute_inv

module RingBoolInv =  InvertMonalg(B)(SB)

let compute_inv_ringbool_tuple = RingInv.compute_inv_tuple
let compute_inv_ringbool = RingInv.compute_inv
                         
let opp_group x =
  match x with
  |Opp g -> g
  |g-> Opp g

(* Given a group element g depending on v, try to compute the inverse of
   v -> g(v) *)
let rec compute_inv (v:var) (g:group) =
  match g with
  | Zero -> None
  | Opp g -> Opt.map opp_group (compute_inv v g)
  | Var x -> if x=v then Some (Var v) else None
  | Add(g1,g2) ->
      match (compute_inv v g1,compute_inv v g2) with
      | None, None -> None
      | Some g, None -> Some (Add (g,opp_group g2))
      | None, Some g -> Some (Add (opp_group g1,g))
      | _,_ -> None
     
