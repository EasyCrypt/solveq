
#use "topfind";;
#require "num";;
#require "batteries";;
#require "menhirLib";;
#require "solveq";;

open Solveq
open Types
open Inverter
open GroebnerBasis
    open Core

let mon_pp = X.pp Format.pp_print_int;;
let bi_pp fmt bi = Format.pp_print_int fmt  (Big_int.int_of_big_int bi);;
let r_pp = R.pp bi_pp;;
let s_pp = S.pp mon_pp r_pp;;
let t_pp = T.pp mon_pp r_pp;;
let p_pp = P.pp s_pp t_pp;;
#install_printer s_pp;;
#install_printer t_pp;;
#install_printer p_pp;;
#install_printer r_pp;;
#install_printer bi_pp;;
#install_printer mon_pp;;
    
let pol = AddR( MultR(VarR 1,VarR 2),VarR 3  );;

compute_inv_ring 1 pol;;

let pol2 = AddR( MultR(VarR 1,VarR 2), MultR(OppR(VarR 3),VarR 1));;
let pol3 = AddR(VarR 3,pol2);;

compute_inv_ring 1 pol3;;

let pol4 = AddR(VarR 1,VarR 3  );;

compute_inv_ring 1 pol4;;


(* Trying to invert (u,v,w,x,y,z) -> (w,v,x-vu,u, y-uw,z-wv) 
 encoded as (1,2,3,4,5,6) -> (3,2,4+21,1,5+13,6-32) 
 the inverse should be (1,2,3,4,5,6) -> (4,2,1,3-24,5-43)
*)


let vars = [1;2;3;4;5;6];;
let pols = [VarR 1;
            VarR 2;
            VarR 3;
            AddR(VarR 4,MultR(VarR 2,VarR 1));
            AddR(VarR 5,MultR(VarR 1,VarR 3));
            AddR(VarR 6,OppR(MultR(VarR 3,VarR 2)))
           ];;  


compute_inv_ring_tuple vars pols;;
(* works when variables well ordered *)


let vars = [11;12;13;14;15;16];;
let pols = [VarR 13;
            VarR 12;
            AddR(VarR 14,MultR(VarR 12,VarR 11));
            VarR 11;
            AddR(VarR 15,MultR(VarR 11,VarR 13));
            AddR(VarR 16,OppR(MultR(VarR 13,VarR 12)))
           ];;  


compute_inv_ring_tuple vars pols;;
(* problem of name management in this one, to work out *)

