(* length and size variable for groups and bitstring *)

type var = int

type group = 
  | Zero
  | Opp of group
  | Add of group*group
  | Var of var

val compute_inv : var -> group -> group option
