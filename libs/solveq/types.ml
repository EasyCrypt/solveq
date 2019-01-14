open Core

type var = int

type group = 
  | Zero
  | Opp of group
  | Add of group*group
  | Var of var

type ring =
  | ZeroR
  | UnitR
  | OppR of ring
  | AddR of ring*ring
  | MultR of ring*ring
  | InvR of ring
  | VarR of var


module R = Monalg.IntField

module V : Monalg.Var with type t = Int.t = struct
  type t = Int.t

  let eq a b = Int.equal a b
  let  compare a b = Int.compare a b
end

module X = Monalg.Multinom(V)  (* the monomials over variables *)
    
module S = Monalg.MonAlg(X)(R) (* polynomials *)

exception NoInv

let rec ring_to_monalg (r:ring) =
    match r with
      | ZeroR -> S.zero
      | UnitR -> S.unit
      | OppR r1 -> S.(~!) (ring_to_monalg r1)
      | AddR (r1,r2) -> S.(+!) (ring_to_monalg r1) (ring_to_monalg r2)
      | MultR (r1,r2)-> S.( *! ) (ring_to_monalg r1) (ring_to_monalg r2)
      | InvR r1 -> raise NoInv
      | VarR x -> S.form (R.unit) (X.ofvar x)

module VMap = Map.Make(V)

let rec var_to_ring (var:var) (pow:int) =
  if pow<0 then
    raise NoInv
  else if pow = 0 then
    UnitR
  else if pow = 1 then
    VarR var
  else
    MultR(VarR var,(var_to_ring var (pow-1)))

let rec monom_to_ring x =
  let map = X.tomap x in
  VMap.fold (fun kn dn a -> match a with
      |UnitR -> var_to_ring kn dn
      |_ ->  MultR((var_to_ring kn dn),a)) map UnitR

let rec monalg_to_ring p =
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
           AddR(monom_to_ring x, monalg_to_ring p)
         else if R.eq r (R.(~!) R.unit) then
           AddR(OppR(monom_to_ring x), monalg_to_ring p)      
         else
           raise NoInv
       end
