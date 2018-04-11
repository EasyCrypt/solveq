(* length and size variable for groups and bitstring *)

type id

type types =
  | BitString of id
  | G of id
  | Fq of id
  | Prod of types list
  | Abstract


type const =
  | Gen       (* a generator of its group types *)
  | F of int  (* a field element *)
  | Zero      (* the empty bitstring of size its types *)

type operator =
  (* bilinear groups *)
  | GExp of id           (* exponentiation in groupe *)
  | GInv                 (* inverse in group *)
  | EMap of id * id *id  (* bilinear map *)
  (* prime field *)
  | FOpp                          (* additive inverse in field *)
  | FMinus                        (* subtraction in field *)
  | FInv                          (* mult. inverse in field *)
  | FDiv                          (* division in field *)
  (* uninterpreted functions and random oracles *)
  | Func  of types          (* function call (uninterpreted) *)
  | FuncBije of types * types (* bijection between types *)

type nary_operator =
  | GMult  (* multiplication in G *)
  | FPlus  (* plus in Fq *)
  | FMult  (* multiplication in Fq *)
  | Xor    (* Xor of bitstrings *)


type var

type terms =
  | V     of var                       (* variables *)
  | Tuple of terms list                (* tuples *)
  | Proj  of int * terms               (* projection *)
  | Const  of const                      (* constants *)
  | App   of operator * terms list     (* fixed arity operators *)
  | Nary  of nary_operator * terms list(* variable arity AC operators *)
