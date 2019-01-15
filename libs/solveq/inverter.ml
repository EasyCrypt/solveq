open Core
open Types
open Monalg
open GroebnerBasis

module Empty = Set.Make(V) (* an empty set *)
    


let rec split_pol (v:var) p1 p2 =
  (* on input p1,p2, splits p1 into p,p' where p' does not depend on v,
     and returns (p,p'+p2) *)
  match (S.split p1) with
  | None -> (S.zero,p2)
  | Some(((x,r),p)) ->
    let (g1,g2) = split_pol v p p2 and m = S.form r x in
    if X.getpow x v != 0 then
      (S.(+!) m g1,g2)
    else
      (g1,S.(+!) m g2)

let rec div_and_indep (v:var) p =
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
let compute_inv_ring (v:var) (r:ring) =
  try
    let p = ring_to_monalg r in (* we put p in normal form, inside a monalg *)
    let (p1,p2)= split_pol v p S.zero in (* we split p in p1+p2, where p2 does not contain v *)
    let p1 = div_and_indep v p1 in (* we divide p1 by v, and obtain a polynom independant from v *)
    (* Here p = v*p1+p2 *)
    let p1 = monalg_to_ring p1 and p2 = monalg_to_ring p2 in
    match p1 with
    |UnitR -> Some(AddR(VarR v, OppR(p2)))
    |_ -> Some(MultR(AddR(VarR v, OppR(p2)), InvR(p1))) (* we return a fraction, might be necessary to check that inv not null *)
  with NoInv -> None


(* Given a list of ring element rs = r1,...,rn depending on a list of variables 
    vs = v1,...,vn , tries to compute the inverse of
   v1,...,vn -> r1,...,rn 
   The inverse is given as a function from VarR 1,...,VarR n -> exprs
   We test the membership of a sub algebra, following a classical GB encoding.
*)
let compute_inv_ring_tuple (vs:var list) (rs:ring list) =
  if List.length vs != List.length rs then raise NoInv;
  try
        let private_vars = ref (Y.of_list vs) in (* we first define every variables as private, not know to the adversary *)
        let counter = ref (0) in 
        let ps = List.fold_left (fun acc elem ->
            counter := !counter+1;
            let fresh_var =  S.form R.unit (X.ofvar (!counter))  in
        match elem with
        | VarR e -> private_vars := Y.remove e (!private_vars);
                    let e = ring_to_monalg (VarR e) in
                    (e, fresh_var)::acc

        | _ ->
          let poly = ring_to_monalg elem in
          let pol_fresh_var =  S.form R.unit (X.ofvar ((-1)*(!counter)))  in
          (S.(-!) poly pol_fresh_var, fresh_var )::acc
      ) [] rs in 

    let basis =  groebner !private_vars ps in
    let inverters = List.map  (
        fun e->  match (deduc !private_vars basis (P.i1 (S.form R.unit (X.ofvar e)))) with
          |None -> raise NoInv
          |Some(q) -> monalg_to_ring q) vs in  (* must check in deduc if the reduced form contains only -t elements *)
    Some(inverters)
  with NoInv -> None



(* Given a group element g depending on v, try to compute the inverse of
   v -> g(v) *)
let rec compute_inv (v:var) (g:group) =
  match g with
  | Zero -> None
  | Opp g -> Opt.map (fun x -> Opp x) (compute_inv v g)
  | Var x -> if x=v then Some (Var v) else None
  | Add(g1,g2) ->
      match (compute_inv v g1,compute_inv v g2) with
      | None, None -> None
      | Some g, None -> Some (Add (g,Opp g2))
      | None, Some g -> Some (Add (Opp g1,g))
      | _,_ -> None
     
