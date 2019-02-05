open Core

type var = int
             
module IV : Monalg.Var with type t = var = struct
  type t = var
             
  let eq a b = a == b
      
  let  compare a b = a - b
      
end 

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

type pvar = String.t
             
module V : Monalg.Var with type t = pvar = struct
  type t = String.t
             
  let eq a b = String.equal a b
      
  let  compare a b = String.compare a b
      
end 

module X = Monalg.Multinom(V)  (* the monomials over variables *)
    
module S = Monalg.MonAlg(X)(R) (* polynomials over intfield *)

module SB = Monalg.MonAlg(X)(B) (* polynomials over field of caracteristic 2 *)

module P = Monalg.ProdAlg(S)(S)

module PB = Monalg.ProdAlg(SB)(SB)

exception NoInv

(* We define a canonical mapping between ring variables (int) and monalg variables (string), mapping variables i to either "xi" for deterministic variable or "zi" for random variables *)
let pvar_of_var ?(pref="x") (x:var) : pvar =
  (String.concat "" [pref;Int.to_string x])

let var_of_pvar (x:pvar) : var =
  Int.of_string (String.sub x 1 ((String.length x)-1))
      
module Converter(R : Monalg.Ring)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t) : sig
  val ring_to_monalg : ?rndvars:Set.Make(IV).t -> ring -> S.t
  val monalg_to_ring : S.t -> ring
  val varset : S.t -> Set.Make(V).t

end =
struct
  module Se = Set.Make(IV)
  let rec ring_to_monalg ?(rndvars=Se.empty) (r:ring) =
    match r with
    | ZeroR -> S.zero
    | UnitR -> S.unit
    | OppR r1 -> S.(~!) (ring_to_monalg r1)
    | AddR (r1,r2) -> S.(+!) (ring_to_monalg r1) (ring_to_monalg r2)
    | MultR (r1,r2)-> S.( *! ) (ring_to_monalg r1) (ring_to_monalg r2)
    | InvR r1 -> raise NoInv
    | VarR x -> let pvar = if Se.mem x rndvars then pvar_of_var ~pref:"z" x else pvar_of_var x in
      S.form (R.unit) (X.ofvar pvar)

  let invert_pvar map pvar =
    let i = var_of_pvar pvar in
    if String.get pvar 0 = 'f' then
      map.(i)
    else
      VarR i

  module VMap = Map.Make(V)

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
    VMap.fold (fun kn dn a ->
        let var = VarR (var_of_pvar kn) in 
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
    let module Se = Set.Make(V) in
    let rec acc (q:S.t) =
      match (S.split q) with
      | None -> Se.empty
      | Some((x,m),r) -> Se.union (X.varset x) (acc r) in
    acc p   
end

module C = Converter(R)(S)
    
