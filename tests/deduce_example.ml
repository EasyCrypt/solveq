
#use "topfind";;
#require "num";;
#require "batteries";;
#require "menhirLib";;
#require "solveq";;

open Solveq
open Types
open Inverter
open GroebnerBasis
open Monalg
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

let [v1;v2;v3;v4;v5;v6] = List.map (fun v-> Var.make_det (Var.of_string v)) ["v1";"v2";"v3";"v4";"v5";"v6"];;


open Monalg_deduce

module DH = Dh_deduce.DeduceDH(R)(S)(P)
module C = Converter(R)(S)
    

  let known_dh = [ExpG (GenG,VarR v2)];;
    
let secrets = [AddR(VarR v1,VarR v2)];;                 

DH.deduce_tuple [v2] known_dh secrets;;


  Big_int.compare_big_int (Big_int.unit_big_int) (Big_int.minus_big_int Big_int.unit_big_int);;

let known_dh = [MultG(ExpG (GenG,VarR v3),ExpG(GenG,OppR(VarR v2))); ExpG (GenG,VarR v2)];;

let secrets = [MultR((VarR v3),VarR v4)];;                 

DH.deduce_tuple [v3;v2] known_dh secrets;;

let secrets = [MultR((VarR v1),AddR(VarR v3,VarR v2))];;                 

DH.deduce_tuple [v3;v2] known_dh secrets;;
