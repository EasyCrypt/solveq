(* -------------------------------------------------------------------- *)
let rec itlist f l b =
  match l with
    [] -> b
  | (h::t) -> f h (itlist f t b);;

let rec lexord ord l1 l2 =
  match (l1,l2) with
    (h1::t1,h2::t2) -> if ord h1 h2 then List.length t1 = List.length t2
                       else h1 = h2 && lexord ord t1 t2
  | _ -> false;;


let rec lexord_lt ord l1 l2 =
  match (l1,l2) with
    ([],[]) -> false
   |([],_) -> true
   |(_,[]) -> false
   | (h1::t1,h2::t2) -> if ord h1 h2 then true
                       else h1 = h2 && lexord_lt ord t1 t2;;

let rec distinctpairs l =
  match l with
   x::t -> itlist (fun y a -> (x,y) :: a) t (distinctpairs t)
  | [] -> [];;

(* -------------------------------------------------------------------- *)
include BatPervasives

(* -------------------------------------------------------------------- *)
module String = BatString
module Int    = BatInt
module Ord    = BatOrd
module Set    = BatSet
module Map    = BatMap
module Num    = BatNum
module Opt    = BatOption

(* -------------------------------------------------------------------- *)
module List : sig
  include module type of BatList

  val lex : ('a -> 'a -> int) -> 'a list -> 'a list -> int
end = struct
  include BatList

  let lex = BatList.compare
end
