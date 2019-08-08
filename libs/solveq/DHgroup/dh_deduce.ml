open Core
open Types
open Monalg


module DeduceDH(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t)(P : Monalg.ProductAlgebra with type ringA = S.t and type ringB = S.t)  = struct
  module C = Converter(R)(S)

  module GB = GroebnerBasis.ProdGB(R)(S)(P)

  
  let rec normalize (g:dhgroup) : S.t =
  match g with
  | UnitG -> S.zero
  | GenG -> S.unit
  | InvG g -> S.(~!)  (normalize g)
  | ExpG (g,r) -> S.( *! )  (C.ring_to_monalg r) (normalize g)
  | MultG (g1,g2) -> S.( *! ) (normalize g1) (normalize g1)

  let deduce_tuple (unknown_vars : var list) (known_dh : dhgroup list) (secrets : ring list) =
    let counter = ref (0) in
   let freshvars = ref VarSet.empty in
(*
    let known_pols = List.map C.ring_to_monalg known_rings in
    let ps = List.fold_left (fun acc poly ->
        counter := !counter+1;
        let fvar = Var.make_fresh (Var.of_int (!counter)) in
        let fresh_var =  S.form R.unit (X.ofvar fvar)  in
        freshvars := VarSet.add fvar (!freshvars);
        (S.(-!) poly fresh_var, fresh_var )::acc
      ) [] known_pols in (* the polynomials that are fully known, the attacker can multiply by them, we use the encoding for sub algebra membership *)
*)
    let known_pols = List.map normalize known_dh in
    let ps = List.fold_left (fun acc poly ->
        counter := !counter+1;
        let fvar = Var.make_fresh (Var.of_int (!counter)) in
        let fresh_var =  S.form R.unit (X.ofvar fvar )  in
        freshvars := VarSet.add fvar (!freshvars);
        (poly, fresh_var )::acc
      ) [(S.unit,S.unit)] known_pols in (* for partially knwon polynomials, we simply add them to the basis *)
    let pvars = VarSet.of_list unknown_vars in
    let basis =  GB.groebner pvars ps in

    let secrets = List.map C.ring_to_monalg secrets in
     let recipees = List.map  (
        fun e->  match (GB.deduc pvars basis e) with
          |None -> raise NoInv
          |Some(q) -> S.(~!) q
            
        ) (List.map (fun x-> (x,S.zero)) secrets)
    in
    (known_pols, ps,basis, recipees)
    

    (* must check in deduc if the reduced form contains only -t elements *)
(*    List.map (fun poly ->
          let module M = Map.Make(X) in
          let varset = C.varset poly in
          if VarSet.subset varset (!freshvars) then (S.(~!) poly)
              else raise NoInv ) recipees
*)
end
