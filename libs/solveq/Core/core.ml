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
  include BatFormat

  type 'a pp = Format.formatter -> 'a -> unit
end

(* --------------------------------------------------------------------- *)


type var = {
  id : int;
  name: string;
  priority:int;
}

module V = struct
  type t = var
    
  let eq v1 v2 = v1.id == v2.id  (* we only compare ids, the rest should not influence equality *)

  let compare v1 v2 =
    if v1.id = v2.id then
      0
    else if v1.priority = v2.priority then
      v1.id - v2.id
    else
      v1.priority - v2.priority
end

module Var = struct
  include V

  module M = Map.Make(String)
      
  let fresh_priority = 0
  let det_priority = 1
  let rnd_priority =2
    
  let id = ref 0
  let map = ref M.empty
      
  let of_string (s:string) = (* we create a fresh id for each new string, using a map to rember the link between strings and variables *)
    try M.find s (!map)
    with Not_found ->
      let p = { id = !id; name = s; priority=det_priority } in
      incr id;
      map := M.add s p (!map);
      p 

  let of_int i =
    of_string (string_of_int i)

  let of_id i =
    let p = { id = i; name = (string_of_int i); priority=det_priority } in
    p
    
  let to_string v = v.name

  let to_int v = int_of_string v.name

  let make_rnd v =
    {v with priority = rnd_priority}

  let make_det v =
    {v with priority = det_priority}
  
  let make_fresh v =
    {v with priority = fresh_priority}

  let pp format v =
    if v.priority = rnd_priority then Format.pp_print_string format "#";
    if v.priority = fresh_priority then Format.pp_print_string format "~";
    Format.fprintf format "%s" v.name
end

module VarSet = Set.Make(Var) 
module VarMap = Map.Make(Var) 
    
