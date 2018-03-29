    	 	
(* the monomial structure (with only variables) *)
module type Monom =
  sig
    type t 
    val zero : t   (* a null set of variables, will be usefull to have "silent" variables equal to zero *)
    val unit : t   (* X^0 *)
    val mult : t -> t -> t 
    val div : t -> t -> t
    val lt : t -> t -> bool  (* lower than ordering *)
    val lcm : t -> t -> t    (* lowest comon multiple *)                 
    val equal : t -> t -> bool
  end
      ;;

(* the ring structure *)
module type Ring =
  sig
    type t
         (* type of elements o the ring *)
    val zero : t
    val unit : t
    val plus : t -> t -> t
    val sub : t -> t -> t
    val mult : t -> t -> t
    val equal : t -> t -> bool
    val print : t -> unit
  end
      ;;
      
(* the polynom structure *)
module type Poly =
  sig
    type m    (* monomials *)
    type c    (* ring coefficent *) 
    type t = (c * m) list    (* a polynom *)
    val zero : t
    val unit : t
    val plus : t -> t -> t
    val mult : t -> t -> t
    val sub : t -> t -> t
    val equal : t -> t -> bool
  end
;;


module Make (M : Monom) (R:Ring) : (Poly with type c = R.t and type m = M.t) =
  struct
    type m = M.t
    type c = R.t
    (* a polynom  is a list of coeff * monomials sorted by the monomial ordering*)
    type t = (c * m) list

    let zero = []

    (*  either all coeff and monomials perfectly match, or we have some null monomials  *)
    let rec equal p1 p2 =
      match p1, p2 with
      | [],[] -> true
      | (c1,m1)::q1, (c2,m2)::q2 -> (R.equal c1 c2 || M.equal m1 M.zero)  && M.equal m1 m2 && equal q1 q2
      | _ -> false
               

    let unit = [R.unit,M.unit]
    let plus p1 p2 = p2
    let mult p1 p2 = p2
    let sub p1 p2 = p2
                       
  end
    ;;


