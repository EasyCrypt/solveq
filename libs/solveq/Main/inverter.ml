open Core
open Types
open Ring_inverter
open Monalg
    
module RingInv =  Ring_inverter.InvertRing(R)(S)

let compute_inv_ring_tuple = RingInv.compute_inv_tuple
let compute_inv_ring = RingInv.compute_inv

module RingBoolInv =  InvertRing(B)(SB)

let compute_inv_ringbool_tuple = RingInv.compute_inv_tuple
let compute_inv_ringbool = RingInv.compute_inv
                         
let opp_group x =
  match x with
  |Opp g -> g
  |g-> Opp g

(* Given a group element g depending on v, try to compute the inverse of
   v -> g(v) *)
let rec compute_inv (v:var) (g:group) =
  match g with
  | Zero -> None
  | Opp g -> Opt.map opp_group (compute_inv v g)
  | Var x -> if x=v then Some (Var v) else None
  | Add(g1,g2) ->
      match (compute_inv v g1,compute_inv v g2) with
      | None, None -> None
      | Some g, None -> Some (Add (g,opp_group g2))
      | None, Some g -> Some (Add (opp_group g1,g))
      | _,_ -> None
     
