open Core
open Monalg
open Types
open Inverter

module Unif(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t)(P : Monalg.ProductAlgebra with type ringA = S.t and type ringB = S.t)
=
struct
  module C = Converter(R)(S)
  module I = Inverter.InvertMonalg(R)(S)
      
  module GB = GroebnerBasis.ProdGB(R)(S)(P)
  module Se = Set.Make(V)

  
  let naive_unif (pols : S.t list) (rndvars : Set.Make(V).t) =
    (* given pols based on some randomvars (included in rndvars) and other vars, try to find a set of random variables which makes pols uniform *)
    let var_pols = List.map (fun acc pol -> Se.inter rndvars (C.varset pol)) Se.empty pols in
    let rnd_var_pols = Se.inter rndvars var_pols in
    let pols_length = List.length pols and rndvars_length = Se.cardinal rnd_var_pols in
    (* here, we need to find a subset R built from the random variables appearing in pols so that its size is equal to the number of pols and the function R -> pols is bijective *)
   ()
    

 end
