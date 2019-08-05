open Core
open Monalg
    
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

let rec pp_ring fmt r : unit =
  match r with
  | ZeroR -> Format.pp_print_int fmt 0
  | UnitR -> Format.pp_print_int fmt 1
  | OppR(r) -> Format.fprintf fmt "(- %a)" pp_ring r
  | AddR(r1,r2) -> Format.fprintf fmt "@[(%a +@ %a)@]" pp_ring r1 pp_ring r2
  | MultR(r1,r2) -> Format.fprintf fmt "@[(%a *@ %a)@]" pp_ring r1 pp_ring r2
  | InvR(r) -> Format.fprintf fmt "(1/%a)" pp_ring r
  | VarR(var) -> Var.pp fmt var

type dhgroup =
  | UnitG
  | GenG
  | InvG of dhgroup
  | MultG of dhgroup * dhgroup
  | ExpG of dhgroup * ring           


exception NoInv

let simp_ring (r:ring) =
  match r with
  | MultR(UnitR,r1) -> r1
  | MultR(r1,UnitR) -> r1
  | AddR(ZeroR,r1) -> r1
  | AddR(r1,ZeroR) -> r1    
  |r -> r

let frac_to_ring (r:ring) = 
  let rec sfrac_to_ring (r:ring) =
    match r with
    |MultR( r1, r2) -> let p1,q1 = sfrac_to_ring r1 and p2,q2 = sfrac_to_ring r2 in
      (MultR(p1,p2),MultR(q1,q2))
    | InvR(r1) ->  let p1,q1 = sfrac_to_ring r1 in
      (q1,p1)
    | AddR (r1,r2) ->  let p1,q1 = sfrac_to_ring r1 and p2,q2 = sfrac_to_ring r2 in
      if q1 =q2 then
        (AddR(p1,p2),q1)
      else raise NoInv
    | r -> (r, UnitR)
  in
  let p,q = (sfrac_to_ring r) in
  simp_ring p,simp_ring q
         

      
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
    | OppR r1 -> S.(~!) (ring_to_monalg ~rndvars:(rndvars) r1)
    | AddR (r1,r2) -> S.(+!) (ring_to_monalg ~rndvars:(rndvars) r1) (ring_to_monalg ~rndvars:(rndvars) r2)
    | MultR (r1,r2)-> S.( *! ) (ring_to_monalg ~rndvars:(rndvars) r1) (ring_to_monalg ~rndvars:(rndvars) r2)
    | InvR _ -> raise NoInv
    | VarR x -> let pvar = if VarSet.exists (fun r -> Var.eq x r) rndvars then Var.make_rnd x else x in
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
    let rec acc (q:S.t) =
      match (S.split q) with
      | None -> VarSet.empty
      | Some((x,m),r) -> VarSet.union (X.varset x) (acc r) in
    acc p    
    
end

module C = Converter(R)(S)
    
