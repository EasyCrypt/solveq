(* Grobner basis computations for K[X]-module *)

#use "topfind";;
#require "num";;
#require "batteries";;
#require "menhirLib"
#cd "../_build/libs/";;
#load "solveq.cmo";;

open Solveq
open GroebnerBasis


let mon_pp = X.pp Format.pp_print_string;;
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

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)


let x = "x" and y = "y" and z = "z";;

let priv = GroebnerBasis.Y.empty;; (* only z is fully known, and only g^x and g^y are known *)
GroebnerBasis.Y.add x priv;;
GroebnerBasis.Y.add y priv;;

let m1 = X.ofvar x ;; (* x *)
let m2 = X.ofvar y;; (* y *)
let m3 = X.ofvar z;; (* z *)
let m4 = X.( *@ ) m1 m2;; (* xy *)
let m5 = X.( *@ ) m2 m3;; (* yz *)

S.form R.unit m2;;
let p1 = S.( +! ) (S.form R.unit m4) (S.form R.unit m2) and sp1 = T.form R.unit (X.ofvar "p1");; (* xy+y *)
let p2 =  (S.form R.unit m4) and sp2 = T.form R.unit (X.ofvar "p2");; (* xy *)


let sp = spoly priv (p1,sp1) (p2,sp2);;

 let gb = groebner priv ([(p1,sp1);(p2,sp2)]);; 

let t = P.i1 p1;;

deduc priv [(p1,sp1)] (P.i1 p1);;
