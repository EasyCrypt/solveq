open Core

type var = {
  id : int;
  name: string;
  priority:int;
}

module Var = struct
  type t = var

  let fresh_priority = 0
  let det_priority = 1
  let rnd_priority =2
    
  let id = ref 0 

  let of_string s =
    let p = { id = !id; name = s; priority=0 } in
    incr id;
    p 

  let of_int s =
    let p = { id = !id; name = (string_of_int s); priority=0 } in
    incr id;
    p

  let to_string v = v.name

  let to_int v = int_of_string v.name
    
  let eq v1 v2 = v1.id == v2.id  (* we only compare ids, the rest should not influence equality *)

  let compare v1 v2 =
    if v1.id = v2.id then
      0
    else if v1.priority = v2.priority then
      v1.id - v2.id
    else
      v1.priority - v2.priority

  let make_rnd v =
    {v with priority = rnd_priority}

  let make_det v =
    {v with priority = det_priority}
  
  let make_fresh v =
    {v with priority = fresh_priority}

  let pp format v =
    Format.pp_print_string format v.name
 
end

module VarSet = Set.Make(Var) 
module VarMap = Map.Make(Var) 
    
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

module B = Monalg.BoolField

module X = Monalg.Multinom(Var)  (* the monomials over variables *)
    
module S = Monalg.MonAlg(X)(R) (* polynomials over intfield *)

module SB = Monalg.MonAlg(X)(B) (* polynomials over field of caracteristic 2 *)

module P = Monalg.ProdAlg(S)(S)

module PB = Monalg.ProdAlg(SB)(SB)

exception NoInv


      
module Converter(R : Monalg.Ring)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t) : sig
  val ring_to_monalg : ?rndvars:VarSet.t -> ring -> S.t
  val monalg_to_ring : S.t -> ring
  val varset : S.t -> VarSet.t

end =
struct
  let rec ring_to_monalg ?(rndvars=VarSet.empty) (r:ring) =
    match r with
    | ZeroR -> S.zero
    | UnitR -> S.unit
    | OppR r1 -> S.(~!) (ring_to_monalg r1)
    | AddR (r1,r2) -> S.(+!) (ring_to_monalg r1) (ring_to_monalg r2)
    | MultR (r1,r2)-> S.( *! ) (ring_to_monalg r1) (ring_to_monalg r2)
    | InvR r1 -> raise NoInv
    | VarR x -> let pvar = if VarSet.mem x rndvars then Var.make_rnd x else Var.make_det x in
      S.form (R.unit) (X.ofvar pvar)



  let rec var_to_ring (var:ring) (pow:int)=
    if pow<0 then
      raise NoInv
    else if pow = 0 then
      UnitR
    else if pow = 1 then
      var
    else
      MultR(var,(var_to_ring var (pow-1)))

  let rec monom_to_ring   x =
    let map = X.tomap x in
    VarMap.fold (fun kn dn a ->
        let var = VarR (kn) in 
        match a with
        |UnitR -> var_to_ring var dn
        |_ ->  MultR((var_to_ring var dn),a)) map UnitR

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


  let varset (p:S.t) =
    let module M = Map.Make(X) in
    let rec acc (q:S.t) =
      match (S.split q) with
      | None -> VarSet.empty
      | Some((x,m),r) -> VarSet.union (X.varset x) (acc r) in
    acc p   
end

module C = Converter(R)(S)
    
