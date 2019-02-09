(* -------------------------------------------------------------------- *)
let rec lexord ord l1 l2 =
  match (l1,l2) with
  | (h1::t1,h2::t2) ->
      if   ord h1 h2
      then List.length t1 = List.length t2
      else h1 = h2 && lexord ord t1 t2
  | _ -> false


let rec lexord_lt ord l1 l2 =
  match (l1,l2) with
   | ([],[]) -> false
   | ([],_ ) -> true
   | (_ ,[]) -> false

   | (h1::t1,h2::t2) ->
       if   ord h1 h2
       then true
       else h1 = h2 && lexord_lt ord t1 t2

(* -------------------------------------------------------------------- *)
include BatPervasives

(* -------------------------------------------------------------------- *)
module String  = BatString
module Int     = BatInt
module Ord     = BatOrd
module Set     = BatSet
module Map     = BatMap
module Num     = BatNum
module Opt     = BatOption
module Hashtbl = BatHashtbl
module IO      = BatIO
module Big_int = BatBig_int
module File = BatFile
  
(* -------------------------------------------------------------------- *)
module List : sig
  include module type of BatList

  val lex : ('a -> 'a -> int) -> 'a list -> 'a list -> int
  val product : 'a list -> ('a * 'a) list
end = struct
  include BatList

  let lex = BatList.compare

  let rec product (xs : 'a list) : ('a * 'a) list =
    match xs with
    | []   -> []
    | x::t -> List.fold_right (fun y a -> (x, y) :: a) t (product t)
end

(* -------------------------------------------------------------------- *)
module Format = struct
  include Format

  type 'a pp = Format.formatter -> 'a -> unit
end
