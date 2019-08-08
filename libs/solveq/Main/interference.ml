open Core
open Monalg
open Types
open GroebnerBasis


module Dependencies(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t)(P : Monalg.ProductAlgebra with type ringA = S.t and type ringB = S.t)
=
struct
  module C = Converter(R)(S)
  module I = Monalg_interference.MonalgDependencies(R)(S)(P)
      
  let check_indep_ring (rings : ring list) (detvars : Set.Make(Var).t) (rndvars : Set.Make(Var).t) =
    let pols =  List.map (C.ring_to_monalg ~rndvars:(rndvars)) rings in
    let detvars = VarSet.fold (fun var acc -> VarSet.add (Var.make_det var) acc ) detvars VarSet.empty and
       rndvars = VarSet.fold (fun var acc -> VarSet.add (Var.make_rnd var) acc ) rndvars VarSet.empty in
    (* Here, we have prefixes to detvars and rndvars so that the ordering invariant required for getdependencies are met *)
    I.check_indep pols detvars rndvars
(* given a list of rings elements, a list of deterministic variables,, either of infinite characteristic or characteristic two, we give back the set of dependent variables *)

end


module RingDep =  Dependencies(R)(S)(P)

let check_indep_ring = RingDep.check_indep_ring

module RingBoolDep =  Dependencies(B)(SB)(PB)

let check_indep_ringbool = RingBoolDep.check_indep_ring
