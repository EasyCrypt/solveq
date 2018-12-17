open Core

type var = int

type group = 
  | Zero
  | Opp of group
  | Add of group*group
  | Var of var

