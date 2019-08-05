open Core
open Types
open Monalg


module DeduceDH(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t)  = struct
  module C = Converter(R)(S)
  
  let rec normalize (g:dhgroup) : S.t =
  match g with
  | UnitG -> S.zero
  | GenG -> S.unit
  | InvG g -> S.(~!)  (normalize g)
  | ExpG (g,r) -> S.( *! )  (C.ring_to_monalg r) (normalize g)
  | MultG (g1,g2) -> S.( *! ) (normalize g1) (normalize g1)

  
end
