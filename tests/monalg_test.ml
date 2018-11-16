#use "topfind";;
#require "num";;
#require "batteries";;
#require "menhirLib"
#cd "../_build/src/";;
#load "solveq.cmo";;

(* -------------------------------------------------------------------- *)
open Utils
open Monalg

(* -------------------------------------------------------------------- *)
module BiVar : sig
  include Var

  val x : t
  val y : t

  val pp : t Format.pp
end = struct
  type t = bool

  let x : t = false
  let y : t = true

  let eq = ((=) : t -> t -> bool)
  let compare = (Pervasives.compare : t -> t -> int)

  let pp (fmt : Format.formatter) (x : t) =
    Format.pp_print_string fmt (if x then "Y" else "X")
end

(* -------------------------------------------------------------------- *)
module Binom = Multinom(BiVar)
module ZXY   = MonAlg(Binom)(IntRing)

(* -------------------------------------------------------------------- *)
let main () =
  let open ZXY in

  let px = ZXY.form Big_int.unit_big_int (Binom.ofvar BiVar.x) in
  let py = ZXY.form Big_int.unit_big_int (Binom.ofvar BiVar.y) in
  let q  = (px -! py) *! (px +! py) in

  Format.fprintf Format.err_formatter "%a\n@."
    (ZXY.pp (Binom.pp BiVar.pp)
            (fun fmt i -> Format.pp_print_string
                            fmt (Big_int.string_of_big_int i)))
    q

(* -------------------------------------------------------------------- *)
let () = main ()

