open Types

val compute_inv : var -> group -> group option

(* inverter for ring elements in ring of infinite characteristic *)
val compute_inv_ring : var -> ring -> ring option

val compute_inv_ring_tuple : var list -> ring list -> ring list option

(* inverters for ring elements in ring of characteristic 2 *)
val compute_inv_ringbool : var -> ring -> ring option

val compute_inv_ringbool_tuple : var list -> ring list -> ring list option
