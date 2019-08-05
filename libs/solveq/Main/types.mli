open Monalg
open Core

(* general type for variable 
type var = {
  id : int;
  name: string;
  priority:int;   (* used to specify that some variables are greater than others *)
}

module Var : sig
  type t = var
  val of_string : string -> var
  val of_int : int -> var
  val of_id : int -> var
  val to_string : var -> string
  val to_int : var -> int
  val eq : var -> var -> bool
  val compare : var -> var -> int
  val make_rnd : var -> var
  val make_det : var -> var
  val make_fresh : var -> var
  val pp :  var Format.pp
   end *)

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

type dhgroup =
  | UnitG
  | GenG
  | InvG of dhgroup
  | MultG of dhgroup * dhgroup
  | ExpG of dhgroup * ring           

val pp_ring : Core.Format.formatter -> ring -> unit

val frac_to_ring : ring -> ring * ring

exception NoInv

(* Types for polynoms in normal form over IntField *)

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
                           val pp : t Core.Format.pp
                         end) ->
  sig
    val ring_to_monalg : ?rndvars:Set.Make(Var).t -> ring -> S.t
    val monalg_to_ring : S.t -> ring
    val varset : S.t -> Core.Set.Make(Var).t
  end

