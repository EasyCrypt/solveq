(* -------------------------------------------------------------------- *)
open Utils

(* -------------------------------------------------------------------- *)
module type Monoid = sig
  type t

  val unit   : t
  val ( *@ ) : t -> t -> t

  include Ord.Eq   with type t := t
  include Ord.Comp with type t := t
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
end

(* -------------------------------------------------------------------- *)
module type Field = sig
  include Ring
 
  val ( /! ) : t -> t -> t
end

(* -------------------------------------------------------------------- *)
module IntField : sig
  include  Field with type t = Big_int.big_int * Big_int.big_int 

  val lcm : t -> t -> t

  val pp : Big_int.big_int Format.pp -> t Format.pp
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
   
  let eq ((p1, q1) : t) ((p2, q2) : t) : bool = beq p1 p2 && beq q1 q2
  let compare ((p1, q1) : t) ((p2, q2) : t) : int = bcompare ( ( *? ) p1 q2) ( ( *? ) q1 p2)

  let pp (ppx : Big_int.big_int Format.pp) (fmt : Format.formatter) ((p, q) : t) =
      Format.fprintf fmt "(%a/%a)" ppx p ppx q


end

(* -------------------------------------------------------------------- *)
module Monom : sig
  include Monoid

  val ofnat : int -> t
  val tonat : t -> int

  val pp : string -> t Format.pp
end = struct
  type t = Monom of int

  let ofnat (x : int) : t =
    Monom (max x 0)

  let tonat (Monom m) : int =
    m

  let unit : t =
    Monom 0

  let ( *@ ) (Monom m1) (Monom m2) : t =
    Monom (m1 + m2)

  let eq =
    ((=) : t -> t -> bool)

  let compare =
    (Pervasives.compare : t -> t -> int)

  let pp (x : string) (fmt : Format.formatter) (Monom m) =
    match m with
    | _ when m <= 0 -> ()
    | 1 -> Format.pp_print_string fmt x
    | _ -> Format.fprintf fmt "%s^%d" x m
end

(* -------------------------------------------------------------------- *)
module type Var = sig
  type t

  include Ord.Eq   with type t := t
  include Ord.Comp with type t := t
end

(* -------------------------------------------------------------------- *)
module Multinom(X : Var)  : sig
  include Monoid

  val getpow : t -> X.t -> int
  val ofvar  : X.t -> t
  val ofmap  : int Map.Make(X).t -> t

  exception DivFailure
  val ( */ )  :  Set.Make(X).t -> t -> t -> t
  val lcm : t -> t -> t

  val pp : X.t Format.pp -> t Format.pp
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
          let xy = x - y  in if (xy < 0 || (xy > 0 && (S.mem v s))) then raise DivFailure else Some xy
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

  let compare (m1 : t) (m2 : t) =
    M.compare (Pervasives.compare) m1 m2

  let pp (ppx : X.t Format.pp) (fmt : Format.formatter) (m : t) =
    let ppcx fmt (x, c) =
      if   c = 1
      then Format.fprintf fmt "%a" ppx x
      else Format.fprintf fmt "%a^%d" ppx x c in

    Format.pp_print_list ~pp_sep:(fun _ () -> ())
      ppcx fmt (M.bindings m)
end

(* -------------------------------------------------------------------- *)
module Seqnom(X : Var) : sig
  include Monoid

  val oflist : X.t list -> t
  val tolist : t -> X.t list

  val pp : X.t Format.pp -> t Format.pp
end = struct
  type t = X.t list

  let oflist (m : X.t list) : t = m
  let tolist (m : t) : X.t list = m

  let unit : t =
    []

  let ( *@ ) (m1 : t) (m2 : t) : t =
    m1 @ m2

  let eq (m1 : t) (m2 : t) =
    List.eq X.eq m1 m2

  let compare (m1 : t) (m2 : t) =
    List.compare X.compare m1 m2

  let pp (ppx : X.t Format.pp) (fmt : Format.formatter) (m : t) =
    Format.pp_print_list ~pp_sep:(fun _ () -> ()) ppx fmt m
end

(* -------------------------------------------------------------------- *)
module MonAlg(X : Monoid)(R : Ring) : sig
  type t

  include Ring with type t := t (* and a bit more :) *)

  val form : R.t -> X.t -> t

  val split : t -> ((X.t * R.t) * t) option  (* return the leading monomial and the remainder *)

  val pp : X.t Format.pp -> R.t Format.pp -> t Format.pp
end = struct
  module M = Map.Make(X)

  type t = R.t M.t

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
      let x, r = M.min_binding p in 
      Some(((x, r), M.remove x p))
    with
        | _ -> None

  let eq (p : t) (q : t) : bool =
    M.equal R.eq p q

  let compare (p : t) (q : t) : int =
    M.compare R.compare p q

  let pp (ppx : X.t Format.pp) (ppc : R.t Format.pp) fmt p =
    let pp_form fmt (f, c) =
      if   R.eq c R.unit
      then ppx fmt f
      else Format.fprintf fmt "%a * %a" ppc c ppx f in

    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " + ")
      pp_form fmt (M.bindings p)
end

(* -------------------------------------------------------------------- *)
module ProdAlg(A : Ring)(B : Ring) : sig
  type t = A.t * B.t

  include Ring with type t := t 

  val pi1 : t -> A.t  
  val pi2 : t -> B.t

  val i1 : A.t -> t
  val i2 : B.t -> t

  val pp : A.t Format.pp -> B.t Format.pp -> t Format.pp
end = struct

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

  let pp (ppx : A.t Format.pp) (ppc : B.t Format.pp) fmt ((p, q) : t) =
    Format.fprintf fmt "(%a, %a)" ppx p ppc q
end


