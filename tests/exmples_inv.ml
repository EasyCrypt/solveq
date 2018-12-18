
#use "topfind";;
#require "num";;
#require "batteries";;
#require "menhirLib";;
#require "solveq";;

open Solveq
open Types
open Inverter

let pol = AddR( MultR(VarR 1,VarR 2),VarR 3  );;

compute_inv_ring 1 pol;;

let pol2 = AddR( MultR(VarR 1,VarR 2), MultR(OppR(VarR 3),VarR 1));;
let pol3 = AddR(VarR 3,pol2);;

compute_inv_ring 1 pol3;;

let pol4 = AddR(VarR 1,VarR 3  );;

compute_inv_ring 1 pol4;;
