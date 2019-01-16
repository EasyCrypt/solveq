(* Grobner basis computations for K[X]-module *)

#use "topfind";;
#require "num";;
#require "batteries";;
#require "menhirLib";;
#require "solveq";;  

open Solveq
open GroebnerBasis
open Types
open Core
    
let mon_pp = X.pp Format.pp_print_string;;
let bi_pp fmt bi = Format.pp_print_int fmt  (Big_int.int_of_big_int bi);;
let r_pp = R.pp bi_pp;;
let s_pp = S.pp mon_pp r_pp;;
let t_pp = T.pp mon_pp r_pp;;
let p_pp = P.pp s_pp s_pp;;
#install_printer s_pp;;
#install_printer t_pp;;
#install_printer p_pp;;
#install_printer r_pp;;
#install_printer bi_pp;;
#install_printer mon_pp;;
(* polynomials displayed from biggest to monom to smallest *)


(* ------------------------------------------------------------------------- *)
(* Examples for Groebner Basis.                                              *)
(* ------------------------------------------------------------------------- *)


let x = "x" and y = "y" and z =  "z";;

let priv = GroebnerBasis.Y.empty;; (* only z is fully known, and only g^x and g^y are known *)
GroebnerBasis.Y.add x priv;;
GroebnerBasis.Y.add y priv;;

let m1 = X.ofvar x ;; (* x *)
let m2 = X.ofvar y;; (* y *)
let m3 = X.ofvar z;; (* z *)
let m4 = X.( *@ ) m1 m2;; (* xy *)
let m5 = X.( *@ ) m2 m3;; (* yz *)
let m6 = X.( *@ ) m1 m1;; (* xx *)

S.form R.unit m2;;
let p1 = S.( +! ) (S.form R.unit m4) (S.form R.unit m2) and sp1 = S.form R.unit (X.ofvar "sp1");; (* xy+y *)
let p2 =  (S.form R.unit m4) and sp2 = S.form R.unit (X.ofvar "sp2");; (* xy *)

let py = (S.form R.unit m2);;

let p4 = S.(+!) p1 (S.form R.unit m6);;
S.split p4;;

X.compare m2 m4;;  (*should be -1 *)
X.compare m6 m1;; (* should be 1 *)
X.compare m1 m2;; (* should be -1*)

S.split p1;;

Int.compare 3 4;;

let sp = spoly priv (p1,sp1) (p2,sp2);;

GroebnerBasis.reduce priv [(p1,sp1);(p2,sp2)] (Opt.get sp);;

let gb = groebner priv ([(p1,sp1);(p2,sp2)]);; 

let t = P.i1 p1;;

deduc priv [(p1,sp1)] (P.i1 p1);;
deduc priv gb (P.i1 py);;

let p,q = Opt.get (GroebnerBasis.reduce priv gb (P.i1 py));;
let Some((x,r),p) = S.split q;;
S.(~!) p;;
let Some((x,r),p) = S.split (S.(~!) p);;
R.(~!) r;;

