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
module Multinom(X : Var) : sig
  include Monoid

  val getpow : t -> X.t -> int
  val ofvar  : X.t -> t
  val ofmap  : int Map.Make(X).t -> t

  val pp : X.t Format.pp -> t Format.pp
end = struct
  module M = Map.Make(X)

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
