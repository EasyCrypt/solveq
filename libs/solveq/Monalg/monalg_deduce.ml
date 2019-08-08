open Core
open Types
open Monalg
open GroebnerBasis

module DeduceMonalg(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t) : sig
  val euclidian_div : var -> S.t -> S.t * S.t
  val deduce_tuple : var list -> S.t list -> S.t list -> S.t list
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
     f(p1,...,pn) = s1,...,sn *)
  let deduce_tuple (pvars:var list) (pols:S.t list) (secrets:S.t list) : S.t list=
    let counter = ref (0) in
    let ps = List.fold_left (fun acc poly ->
        counter := !counter+1;
        let fresh_var =  S.form R.unit (X.ofvar (Var.make_fresh (Var.of_int (!counter)) ))  in
        (S.(-!) poly fresh_var )::acc
      ) [] pols in 
    
    let basis =  GB.groebner VarSet.empty ps in
    let recipees = List.map  (
        fun e->  match (GB.deduc VarSet.empty basis e) with
          |None -> raise NoInv
          |Some(q) -> q
            
        ) secrets
    in  

    (* must check in deduc if the reduced form contains only -t elements *)
    let private_vars = (VarSet.of_list pvars) in (* we thus define every variables as private, not know to the adversary *)
    List.map (fun poly ->
          let module M = Map.Make(X) in
          let varset = C.varset poly in
          if VarSet.is_empty (VarSet.inter varset private_vars) then (S.(~!) poly)
              else raise NoInv ) recipees
  
  (* given a function v1,..,vn -> p1,...,pn, computes f:x1,...,xn -> e1,..,en such that
     f(p1,...,pn) = v1,...,vn *)
  let inverter_tuple (vars:var list) (pols:S.t list) : S.t list=
    if List.length vars != List.length pols then raise NoInv;
    let secrets = List.map (fun e -> S.form R.unit (X.ofvar (e))) vars in
    deduce_tuple vars pols secrets
end
