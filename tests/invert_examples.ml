
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


let var_pp = Var.pp;;
let mon_pp = X.pp;;
let b_pp = B.pp;;
let r_pp = R.pp;;
let s_pp = S.pp;;
let p_pp = P.pp;;
let sb_pp = SB.pp;;
#install_printer var_pp;;
#install_printer s_pp;;
#install_printer p_pp;;
#install_printer r_pp;;
#install_printer bi_pp;;
#install_printer sb_pp;;
#install_printer mon_pp;;

let [v1;v2;v3;v4;v5;v6] = List.map (fun v-> Var.make_det (Var.of_int v)) [1;2;3;4;5;6];;

let g = Add(Var v1,Var v2);;

compute_inv v1 g;;

module C = Converter(R)(S)
  module Inv = InvertMonalg(R)(S)


let g2 = Add(Var v1,Opp(Var v2));;

compute_inv v1 g2;;



let pol = AddR( MultR(VarR v1,VarR v2),VarR v3  );;

let test = C.ring_to_monalg pol;;


compute_inv_ring v2 pol;;

let pol2 = AddR( MultR(VarR v1,VarR v2), MultR(OppR(VarR v3),VarR v1));;
let pol3 = AddR(VarR v3,pol2);;

compute_inv_ring v1 pol3;;

let pol4 = AddR(VarR v1,VarR v3  );;

compute_inv_ring v1 pol4;;


(* Trying to invert (u,v,w,x,y,z) -> (w,v,x-vu,u, y-uw,z-wv) 
 encoded as (1,2,3,4,5,6) -> (3,2,4+21,1,5+13,6-32) 
 the inverse should be (1,2,3,4,5,6) -> (4,2,1,3-24,5-43)
*)

let vars = [v1;v2;v3;v4;v5;v6];;
let pols = [VarR v1;
            VarR v2;
            VarR v3;
            AddR(VarR v4,MultR(VarR v2,VarR v1));
            AddR(VarR v5,MultR(VarR v1,VarR v3));
            AddR(VarR v6,OppR(MultR(VarR v3,VarR v2)))
           ];;  

compute_inv_ring_tuple vars pols;;
(* very similar to cramer shoup *)

let [v11;v12;v13;v14;v15;v16] = List.map (fun v-> Var.make_det (Var.of_int v)) [11;12;13;14;15;16];;

let vars2 = [v11;v12;v13;v14;v15;v16];;
let pols2 = [VarR v13;
            VarR v12;
            AddR(VarR v14,MultR(VarR v12,VarR v11));
            VarR v11;
            AddR(VarR v15,MultR(VarR v11,VarR v13));
            AddR(VarR v16,OppR(MultR(VarR v13,VarR v12)))
           ];;  


compute_inv_ring_tuple vars2 pols2;;

let vars3 = [v1;v2;v3];;
let pols3 = [VarR v1;
            VarR v2;
            MultR(VarR v3,VarR v2);
           ];;  

compute_inv_ring_tuple vars3 pols3;; (* should return None *)

let pols4 = [VarR v1;
            AddR(VarR v2,MultR(VarR v1,VarR v3));
            AddR(VarR v3,OppR(AddR(VarR v2,MultR(VarR v1,VarR v3))));
            ];;  (*x, y +xz, z-y-xz*)

compute_inv_ring_tuple vars3 pols4;; (* correct inverse: x, -xz -xy + y   ,y+z *)
