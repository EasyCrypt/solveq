open Core
open Types
open Monalg


module V : Monalg.Var with type t = Int.t = struct
  type t = Int.t

  let eq a b = Int.equal a b
  let  compare a b = Int.compare a b
end

module R = Monalg.IntField 
module X = Monalg.Multinom(V)  (* the monomials over variables *)
module S = Monalg.MonAlg(X)(R) (* polynomials *)
module Empty = Set.Make(V) (* an empty set *)
module VMap = Map.Make(V)
    
exception NoInv
let rec ring_to_monal (r:ring) =
    match r with
      | ZeroR -> S.zero
      | UnitR -> S.unit
      | OppR r1 -> S.(~!) (ring_to_monal r1)
      | AddR (r1,r2) -> S.(+!) (ring_to_monal r1) (ring_to_monal r2)
      | MultR (r1,r2)-> S.( *! ) (ring_to_monal r1) (ring_to_monal r2)
      | InvR r1 -> raise NoInv
      | VarR x -> S.form (R.unit) (X.ofvar x)

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

let rec varpow_to_ring var pow =
  if pow<0 then
    raise NoInv
  else if pow = 0 then
    UnitR
  else if pow = 1 then
    VarR var
  else
    MultR(VarR var,(varpow_to_ring var (pow-1)))

let rec monom_to_ring x =
  let map = X.tomap x in
  VMap.fold (fun kn dn a -> match a with
      |UnitR -> varpow_to_ring kn dn
      |_ ->  MultR((varpow_to_ring kn dn),a)) map UnitR

let rec pol_to_ring p =
 match (S.split p) with
   | None -> ZeroR     
   | Some(((x,r),p)) ->
     if S.eq p S.zero then
       begin
         if R.eq r R.unit then
           monom_to_ring x
         else if R.eq r (R.(~!) R.unit) then
           OppR(monom_to_ring x)      
         else
           raise NoInv
       end
     else
       begin
         if R.eq r R.unit then
           AddR(monom_to_ring x, pol_to_ring p)
         else if R.eq r (R.(~!) R.unit) then
           AddR(OppR(monom_to_ring x), pol_to_ring p)      
         else
           raise NoInv
       end
        
let compute_inv_ring (v:var) (r:ring) =
  try
    let p = ring_to_monal r in (* we put p in normal form, inside a monalg *)
    let (p1,p2)= split_pol v p S.zero in (* we split p in p1+p2, where p2 does not contain v *)
    let p1 = div_and_indep v p1 in (* we divide p1 by v, and obtain a polynom independant from v *)
    (* Here p = v*p1+p2 *)
    let p1 = pol_to_ring p1 and p2 = pol_to_ring p2 in
    match p1 with
    |UnitR -> Some(AddR(VarR v, OppR(p2)))
    |_ -> Some(MultR(AddR(VarR v, OppR(p2)), InvR(p1))) (* we return a fraction, might be necessary to check that inv not null *)
  with NoInv -> None


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
     
