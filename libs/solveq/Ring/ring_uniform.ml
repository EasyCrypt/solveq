open Monalg
open Types
open Core
    
module RingUniform(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t)(P : Monalg.ProductAlgebra with type ringA = S.t and type ringB = S.t)
=
struct
  module C = Converter(R)(S)
  module U = Monalg_uniform.MonalgUnif(R)(S)(P)

   let is_unif_ring (rings : ring list) (detvars : Set.Make(Var).t) (rndvars : Set.Make(Var).t) =
    let pols =  List.map (C.ring_to_monalg ~rndvars:(rndvars)) rings in
    let detvars = VarSet.fold (fun var acc -> VarSet.add (Var.make_det var) acc ) detvars VarSet.empty and
       rndvars = VarSet.fold (fun var acc -> VarSet.add (Var.make_rnd var) acc ) rndvars VarSet.empty in
    (* Here, we have prefixes to detvars and rndvars so that the ordering invariant required for getdependencies are met *)
    U.naive_is_unif pols rndvars

end
