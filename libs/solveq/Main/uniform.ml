open Monalg
    
module Unif = Ring_uniform.RingUniform
                
module RingU =  Unif(R)(S)(P)

let is_unif_ring = RingU.is_unif_ring

module RingBoolU =  Unif(B)(SB)(PB)

let is_unif_ringbool = RingBoolU.is_unif_ring
