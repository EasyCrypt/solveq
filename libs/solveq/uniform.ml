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
  module VarSet = Set.Make(Var)
  module VarSetSet = Set.Make(VarSet)

  exception NoSubSet

  let rec all_sub_sets sets =
    (* given a list of sets s1,..,sk, computes a set of all the set S of size n such that S is a subset of the union of all si, and the intersection between and S and each si is exactly one. We must select a distinct element from each si. *)
    match sets with
    |[] -> VarSetSet.empty
    |[p] -> VarSet.fold (fun var acc -> VarSetSet.add (VarSet.singleton var) acc) p VarSetSet.empty
    |p::q ->
          let subsets = all_sub_sets q in
          (* we compute all the possible subsets for the remainder *)

          VarSet.fold (fun var acc -> 
              let acceptable_subsets = VarSetSet.filter (fun set -> not(VarSet.mem var set)) subsets in
              (* we can only build a set extended with var if var is not in the subset *)
              (* then, to each accpetable subset, we add the var to it, and then we add all those new sets to the acc *)
              VarSetSet.union acc (VarSetSet.map (fun set -> VarSet.add var set) acceptable_subsets)
          ) p VarSetSet.empty


  module M = Map.Make(Var)
  let rec set_mon_rnd  mon rndvars = (* are the VarSet in the following correct ? Need to check if mem uses the eq of Var, but probably not ... *)
    let map = X.tomap mon in
    let nmap = M.fold (fun v i acc -> if VarSet.mem v rndvars then M.add (Var.make_rnd v) i acc else M.add (Var.make_det v) i acc ) map M.empty in
    X.ofmap nmap
      

  let rec set_pol_rnd pol rndvars =
    match (S.split pol) with
    |None -> S.zero
    |Some((mon,r),remainder) -> S.( +! ) (S.form r (set_mon_rnd mon rndvars)) (set_pol_rnd remainder rndvars)

  let naive_is_unif (pols : S.t list) (rndvars : Set.Make(Var).t) =
    (* given pols based on some randomvars (included in rndvars) and other vars, try to find a set of random variables which makes pols uniform *)
    (* is complete only if the number of pols is equal to the number of rndvars *)
    let var_pols = List.map (fun pol -> VarSet.inter rndvars (C.varset pol)) pols in
    let pols_length = List.length pols in
    (* here, we need to find a subset R built from the random variables appearing in pols so that its size is equal to the number of pols and the function R -> pols is bijective *)
    let subrndvars = all_sub_sets var_pols in
    let rec is_unif varsubsets =
      if VarSetSet.is_empty varsubsets then false
      else
        begin
          let p,q =VarSetSet.pop varsubsets in
          (* we change the status of the different variables to concord with the current subset *)
          try
            let newpols = List.map (fun pol -> set_pol_rnd pol p) pols and newrndvars = VarSet.map Var.make_rnd p in
            let inverters = I.inverter_tuple (VarSet.to_list newrndvars) newpols in
            (* we reset the status of the variables *)
            VarSet.iter (fun v -> Var.make_rnd v; ()) rndvars;             
            Format.printf "    -> uniformity witness: ( ";
            VarSet.iter (fun r -> Format.printf " %a " Var.pp r) newrndvars;
            Format.printf "->";
            List.iter (fun r -> Format.printf " %a " S.pp r) newpols;
            Format.printf ") (";
            List.iteri (fun i _ -> Format.printf " %a " Var.pp (Var.make_fresh (Var.of_int (i+1)))) inverters;
            Format.printf "->";
            List.iter (fun r -> Format.printf " %a " S.pp r) inverters;
            Format.printf ")@.";
            true
          with NoInv -> is_unif q
          end
    in
    is_unif subrndvars

   let is_unif_ring (rings : ring list) (detvars : Set.Make(Var).t) (rndvars : Set.Make(Var).t) =
    let pols =  List.map (C.ring_to_monalg ~rndvars:(rndvars)) rings in
    let detvars = VarSet.fold (fun var acc -> VarSet.add (Var.make_det var) acc ) detvars VarSet.empty and
       rndvars = VarSet.fold (fun var acc -> VarSet.add (Var.make_rnd var) acc ) rndvars VarSet.empty in
    (* Here, we have prefixes to detvars and rndvars so that the ordering invariant required for getdependencies are met *)
    naive_is_unif pols rndvars

 end



module RingU =  Unif(R)(S)(P)

let is_unif_ring = RingU.is_unif_ring

module RingBoolU =  Unif(B)(SB)(PB)

let is_unif_ringbool = RingBoolU.is_unif_ring
