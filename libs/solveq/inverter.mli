open Types

module InvertMonalg : functor
  (R : Monalg.Field) (S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t) ->
  sig
    val split_pol : var -> S.t -> S.t * S.t
    val euclidian_div : var -> S.t -> S.t * S.t
    val inverter_tuple : var list -> S.t list -> S.t list
  end

val compute_inv : var -> group -> group option

(* inverter for ring elements in ring of infinite characteristic *)
val compute_inv_ring : var -> ring -> ring option

val compute_inv_ring_tuple : var list -> ring list -> ring list option

(* inverters for ring elements in ring of characteristic 2 *)
val compute_inv_ringbool : var -> ring -> ring option

val compute_inv_ringbool_tuple : var list -> ring list -> ring list option
