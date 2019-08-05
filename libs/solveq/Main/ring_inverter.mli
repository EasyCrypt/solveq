open Core
open Monalg
open Types
    
module InvertMonalg : functor
  (R : Monalg.Field) (S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t) ->
  sig
    val euclidian_div : var -> S.t -> S.t * S.t
    val inverter_tuple : var list -> S.t list -> S.t list
  end

module InvertRing : functor
   (R : Monalg.Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t) -> sig
 val compute_inv : var -> ring -> ring option
 val compute_inv_tuple : var list -> ring list ->  ring list option
end
