open Monalg
open Core

(* general type for variable *)
type var = {
  id : int;
  name: string;
  priority:int;   (* used to specify that some variables are greater than others *)
}

module Var : sig
  type t = var
  val of_string : string -> var
  val of_int : int -> var
  val to_string : var -> string
  val to_int : var -> int
  val eq : var -> var -> bool
  val compare : var -> var -> int
  val make_rnd : var -> var
  val make_det : var -> var
  val make_fresh : var -> var
  val pp :  Core.Format.formatter -> var -> unit
end

(* Abstract types for abelian group or ring *)
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

exception NoInv

(* Types for polynoms in normal form over IntField *)

module R = IntField   (* module for fields elements *)

module B = BoolField (* finite field of characteristic 2 *)

module X : sig  (* module for monomials *)
  type t = Monalg.Multinom(Var).t
  val unit : t
  val ( *@ ) : t -> t -> t
  val eq : t Core.Ord.eq
  val compare : t Core.Ord.comp
  val getpow : t -> Var.t -> int
  val ofvar : Var.t -> t
  val ofmap : int Core.Map.Make(Var).t -> t
  val tomap : t -> int Core.Map.Make(Var).t
  val varset : t -> Set.Make(Var).t
  exception DivFailure
  val ( */ ) : Core.Set.Make(Var).t -> t -> t -> t
  val lcm : t -> t -> t
  val pp : Var.t Core.Format.pp -> t Core.Format.pp
end
    
module S :Monalg.MonAlgebra with type ring = R.t and type mon = X.t
                                                                  
module SB : sig (* module for polynomials over finite field of characteristic 2 *)
  type t = Monalg.MonAlg(X)(B).t
  type ring = B.t
  type mon = X.t
  val zero : t
  val unit : t
  val ( +! ) : t -> t -> t
  val ( -! ) : t -> t -> t
  val ( ~! ) : t -> t
  val ( *! ) : t -> t -> t
  val eq : t Core.Ord.eq
  val compare : t Core.Ord.comp
  val form : ring -> mon -> t
  val split : t -> ((mon * ring) * t) option
  val pp : X.t Core.Format.pp -> B.t Core.Format.pp -> t Core.Format.pp
end

module P : Monalg.ProductAlgebra with type ringA = S.t and type ringB = S.t

module PB : Monalg.ProductAlgebra with type ringA = SB.t and type ringB = SB.t

module Converter : functor
  (R : Monalg.Ring) (S : sig
                           type t
                           type ring = R.t
                           type mon = X.t
                           val zero : t
                           val unit : t
                           val ( +! ) : t -> t -> t
                           val ( -! ) : t -> t -> t
                           val ( ~! ) : t -> t
                           val ( *! ) : t -> t -> t
                           val eq : t Core.Ord.eq
                           val compare : t Core.Ord.comp
                           val form : ring -> mon -> t
                           val split : t -> ((mon * ring) * t) option
                           val pp :
                             mon Core.Format.pp ->
                             ring Core.Format.pp -> t Core.Format.pp
                         end) ->
  sig
    val ring_to_monalg : ?rndvars:Set.Make(Var).t -> ring -> S.t
    val monalg_to_ring : S.t -> ring
    val varset : S.t -> Core.Set.Make(Var).t
  end

