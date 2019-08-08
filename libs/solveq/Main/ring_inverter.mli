open Core
open Monalg
open Types
    
module InvertRing : functor
   (R : Monalg.Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t) -> sig
 val compute_inv : var -> ring -> ring option
 val compute_inv_tuple : var list -> ring list ->  ring list option
end
