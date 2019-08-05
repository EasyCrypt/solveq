(* -------------------------------------------------------------------- *)
open Core

(* -------------------------------------------------------------------- *)
module type Monoid = sig
  type t

  val unit   : t
  val ( *@ ) : t -> t -> t

  include Ord.Eq   with type t := t
  include Ord.Comp with type t := t

  val pp : t Format.pp
end

(* -------------------------------------------------------------------- *)
module type Ring = sig
  type t

  val zero  : t
  val unit  : t

  val ( +! ) : t -> t -> t
  val ( -! ) : t -> t -> t
  val ( ~! ) : t -> t
  val ( *! ) : t -> t -> t
  include Ord.Eq   with type t := t
  include Ord.Comp with type t := t

  val pp : t Format.pp

end

(* -------------------------------------------------------------------- *)
module IntRing : Ring with type t = Big_int.big_int = struct

  type t = Big_int.big_int

  let zero : t = Big_int.zero_big_int
  let unit : t = Big_int.unit_big_int

  let ( +! ) = Big_int.add_big_int
  let ( -! ) = Big_int.sub_big_int
  let ( ~! ) = Big_int.minus_big_int
  let ( *! ) = Big_int.mult_big_int

  let eq = Big_int.eq_big_int
  let compare = Big_int.compare_big_int

  let pp fmt bi = Format.fprintf fmt "%s" (Big_int.to_string bi)

end

(* -------------------------------------------------------------------- *)
module type BigIntVal = sig
  val x : Big_int.big_int
end

module type Modulo = sig
  val modulo : Big_int.big_int -> Big_int.big_int
end

module Modulo(B : BigIntVal) : Modulo = struct
  let modulo x = Big_int.modulo x B.x
end

module BoolBigInt : BigIntVal = struct let x = Big_int.big_int_of_int 2 end

module BoolMod = Modulo(BoolBigInt)

(* -------------------------------------------------------------------- *)
module type Field = sig
  include Ring
 
  val ( /! ) : t -> t -> t

  val lcm : t -> t -> t

end

(* -------------------------------------------------------------------- *)
module FiniteField(M : Modulo) : sig
  include Field with type t = Big_int.big_int
             
end = struct
  type t = Big_int.big_int

  let zero : t = Big_int.zero_big_int
  let unit : t = Big_int.unit_big_int

  let ( +! ) = fun x y -> M.modulo (Big_int.add_big_int x y)
  let ( -! ) = fun x y -> M.modulo (Big_int.sub_big_int x y)
  let ( ~! ) = fun x -> M.modulo (Big_int.minus_big_int x)
  let ( *! ) = fun x y -> M.modulo (Big_int.mult_big_int x y)
  let (/!) = fun x y ->  Big_int.div_big_int (M.modulo x) (M.modulo y)
      
  let eq = fun x y -> Big_int.eq_big_int (M.modulo x) (M.modulo y)
  let compare = fun x y -> Big_int.compare_big_int (M.modulo x) (M.modulo y)

  let lcm = fun x y -> if eq (M.modulo x) unit && eq (M.modulo y) unit then unit else zero

  let pp = IntRing.pp 

end

module BoolField = FiniteField(BoolMod)

module IntField : sig
  include  Field with type t = Big_int.big_int * Big_int.big_int 

end = struct

  type v = Big_int.big_int
  type t = v * v

  let bzero = Big_int.zero_big_int
  let bunit = Big_int.unit_big_int
  let ( +? ) = Big_int.add_big_int
  let ( -? ) = Big_int.sub_big_int
  let ( ~? ) = Big_int.minus_big_int
  let ( *? ) = Big_int.mult_big_int
  let ( /? ) = Big_int.div_big_int
  let beq = Big_int.eq_big_int
  let bcompare = Big_int.compare_big_int

  let zero : t = (bzero, bunit)
  let unit : t = (bunit, bunit)


  let rec bgcd (u : v) (v : v) =
  	if not (v == bzero) then (bgcd v (Big_int.mod_big_int u v)) else (Big_int.abs_big_int u) 

  let blcm m n =
  	match m, n with
	  | zero, _ | _, zero -> zero
	  | m, n -> (/?) (Big_int.abs_big_int ( ( *? ) m n)) (bgcd m n)

  let norm ((p,q) : t) : t = 
    if beq p bzero then zero else 
        let m = blcm p q in
		    ((/?) p m, (/?) q m)		

  let ( +! ) ((p1, q1) : t) ((p2, q2) : t) : t =
	norm ( (+?) ( ( *?) p1 q2) ( ( *? ) p2 q1), ( *? ) q1 q2) 
  let ( -! ) ((p1, q1) : t) ((p2, q2) : t) : t =
	norm ( (-?) ( ( *?) p1 q2) ( ( *? ) p2 q1), ( *? ) q1 q2) 
  let ( ~! ) ((p, q) : t) =  ((~?) p, q)
  let ( *! ) ((p1, q1) : t) ((p2, q2) : t) : t =
	norm (( *?) p1 p2, ( *? ) q1 q2) 
  let ( /! ) ((p1, q1) : t) ((p2, q2) : t) : t =
	norm (( *?) p1 q2, ( *? ) q1 p2) 
  
  let lcm ((p1, q1) : t) ((p2, q2) : t) : t = 
    norm (blcm p1 p2, ( *?) q1 q2)
   
  let eq ((p1, q1) : t) ((p2, q2) : t) : bool = (beq p1 p2 && beq q1 q2)
                                                || ((beq p1 ((~?) p2)) && beq  q1 ((~?) q2) )
  let compare ((p1, q1) : t) ((p2, q2) : t) : int = bcompare ( ( *? ) p1 q2) ( ( *? ) q1 p2)

  let pp (fmt : Format.formatter) ((p, q) : t) =
    let ppx = IntRing.pp in
    Format.fprintf fmt "(%a/%a)" ppx p ppx q


end

(* -------------------------------------------------------------------- *)
module type Var = sig
  type t

  include Ord.Eq   with type t := t
  include Ord.Comp with type t := t
  
  val pp : t Format.pp
end

(* -------------------------------------------------------------------- *)
module Multinom(X : Var)  : sig
  include Monoid

  val getpow : t -> X.t -> int
  val ofvar  : X.t -> t
  val ofmap  : int Map.Make(X).t -> t
  val tomap  : t-> int Map.Make(X).t
  val varset : t -> Set.Make(X).t
  exception DivFailure
  val ( */ )  :  Set.Make(X).t -> t -> t -> t
  val lcm : t -> t -> t
  val compare : t -> t -> int
end = struct
  module M = Map.Make(X)
  module S = Set.Make(X)
  type t = int M.t

  let getpow (m : t) (i : X.t) =
    M.find_default 0 i m

  let ofvar (x : X.t) =
    M.singleton x 1

  let ofmap (m : int M.t) : t =
    M.filter (fun _ i -> 0 < i) m

  let tomap (m : t) : int M.t =
    m
  let varset (m:t) =
     S.of_list (List.map (fun (u,v)->u) (M.bindings m))    

  let unit : t =
    M.empty

  let ( *@ ) (m1 : t) (m2 : t) =
    let merge i1 i2 =
      match i1, i2 with
      | None  , None   -> None
      | Some _, None   -> i1
      | None  , Some _ -> i2
      | Some x, Some y ->
          let xy = x + y in if xy = 0 then None else Some xy
    in M.merge (fun _ -> merge) m1 m2

  exception DivFailure

  let ( */ ) (s : S.t) (m1 : t) (m2 : t) =
    let merge v i1 i2 =
      match i1, i2 with
      | None  , None   -> None
      | Some _, None   -> i1
      | None  , Some _ -> raise DivFailure
      | Some x, Some y ->
          let xy = x - y  in 
            if xy = 0 then None
            else if (xy < 0 || (S.mem v s)) then 
                raise DivFailure 
            else Some xy
    in M.merge merge m1 m2

  let lcm (m1 : t) (m2 : t) =
    let merge i1 i2 =
      match i1, i2 with
      | None  , None   -> None
      | Some _, None   -> i1
      | None  , Some _ -> i2
      | Some x, Some y ->
          let xy = max x y  in if xy = 0 then None else Some xy
    in M.merge (fun _ -> merge) m1 m2

  let eq (m1 : t) (m2 : t) =
    M.equal (=) m1 m2

  let rec compare (m1 : t) (m2 : t) =
    if m1 = unit && m2 = unit then 0
    else if m1 = unit then -1
    else if m2 = unit then 1
    else
      (
        let (x1,p1) = M.max_binding m1 and (x2,p2) = M.max_binding m2 in
        let comp  = X.compare x1 x2 in
        if comp = 0 then
          (
            if p1 < p2 then -1
            else if p1 > p2 then 1
            else compare (M.remove x1 m1 ) (M.remove x2 m2)
          )
        else comp
  )
    (* correspond to the lexicographic order on multinoms 
                                           *)
  let pp (fmt : Format.formatter) (m : t) =
    let ppx = X.pp in
    let ppcx fmt (x, c) =
      if   c = 1
      then Format.fprintf fmt "%a" ppx x
      else Format.fprintf fmt "%a^%d" ppx x c in

    Format.pp_print_list ~pp_sep:(fun _ () -> ())
      ppcx fmt (List.rev (M.bindings m))
end


(* -------------------------------------------------------------------- *)
module type MonAlgebra = sig
  type t
  type ring
  type mon
  include Ring with type t := t

  val form : ring -> mon -> t

  val split : t -> ((mon * ring) * t) option  (* return the leading monomial and the remainder *)

  end

module MonAlg(X : Monoid)(R : Ring) : sig
  type t

  include MonAlgebra with type t := t and type ring = R.t and type mon = X.t (* and a bit more :) *)

  end
 = struct
  module M = Map.Make(X)
  module S = Set.Make(X)

  type t = R.t M.t
  type ring = R.t
  type mon = X.t
  
  let norm (x : R.t) : R.t option =
    if R.eq x R.zero then None else Some x

  let dfl (x : R.t option) : R.t =
    Opt.default R.zero x

  let zero : t =
    M.empty

  let unit : t =
    M.singleton X.unit R.unit
    
  let form (c : R.t) (x : X.t) : t =
    match norm c with
    | None   -> zero
    | Some c -> M.singleton x c

  let ( +! ) (p : t) (q : t) : t =
    let merge i j = norm (R.(+!) (dfl i) (dfl j)) in
    M.merge (fun _ -> merge) p q

  let ( -! ) (p : t) (q : t) : t =
    let merge i j = norm (R.(-!) (dfl i) (dfl j)) in
    M.merge (fun _ -> merge) p q

  let ( ~! ) (p : t) : t =
    M.map R.(~!) p

  let ( *! ) (p : t) (q : t) : t =
    (* Pay your complexity... *)
    let add1 (c, x) (c', x') r =
      r +! form (R.( *! ) c c') (X.( *@ ) x x') in

    M.fold
      (fun x c -> M.fold (fun x' c'-> add1 (c, x) (c', x')) q)
      p zero

  let split (p : t) : ((X.t * R.t) * t) option =
    try 
      let x, r = M.max_binding p in 
      Some(((x, r), M.remove x p))
    with
        | _ -> None

  let eq (p : t) (q : t) : bool =
    M.equal R.eq p q

  let compare (p : t) (q : t) : int =
    M.compare R.compare p q

  let pp fmt p =
    let ppx = X.pp in
    let ppc = R.pp in
    let pp_form fmt (f, c) =
      if   R.eq c R.unit
      then ppx fmt f
      else Format.fprintf fmt "%a * %a" ppc c ppx f in

    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " + ")
      pp_form fmt (List.rev (M.bindings p))
end

(* -------------------------------------------------------------------- *)
module type ProductAlgebra = sig
  type ringA
  type ringB
  type t = ringA * ringB

  include Ring with type t := t 

  val pi1 : t -> ringA
  val pi2 : t -> ringB

  val i1 : ringA -> t
  val i2 : ringB -> t

end

module ProdAlg(A : Ring)(B : Ring) : ProductAlgebra with type ringA = A.t and type ringB = B.t = struct

  type ringA = A.t
  type ringB = B.t
  type t = A.t * B.t

  let pi1 ((p, q) : t) : A.t = p

  let pi2 ((p, q) : t) : B.t = q

  let i1 (a : A.t) : t = (a, B.zero)
  
  let i2 (b : B.t) : t = (A.zero, b)
 
  let zero : t =
    A.zero, B.zero

  let unit : t =
    A.unit, B.unit

  let ( +! ) ((p1, q1) : t) ((p2, q2) : t) : t =
    (A.(+!) p1 p2, B.(+!) q1 q2)

  let ( -! ) ((p1, q1) : t) ((p2, q2) : t) : t =
    (A.(-!) p1 p2, B.(-!) q1 q2)

  let ( ~! ) ((p, q) : t) : t =
    (A.(~!) p, B.(~!) q)

  let ( *! ) ((p1, q1) : t) ((p2, q2) : t) : t =
    (A.( *! ) p1 p2, B.( *! ) q1 q2)

  let eq ((p1, q1) : t) ((p2, q2) : t) : bool =
    (A.eq p1 p2) && (B.eq q1 q2)

  let compare ((p1, q1) : t) ((p2, q2) : t) : int =
    let x = A.compare p1 p2 in
       if x <> 0 then x else (B.compare q1 q2)

  let pp fmt ((p, q) : t) =
    Format.fprintf fmt "(%a, %a)" A.pp p B.pp q
end


module R = IntField

module B = BoolField

module X = Multinom(Var)  (* the monomials over variables *)
    
module S = MonAlg(X)(R) (* polynomials over intfield *)

module SB = MonAlg(X)(B) (* polynomials over field of caracteristic 2 *)

module P = ProdAlg(S)(S)

module PB = ProdAlg(SB)(SB)
