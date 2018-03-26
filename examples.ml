(* Grobner basis computations for K[X]-module *)

  #use "topfind";;
#require "num";;
#cd "_build/src/";;
#load "utils.cmo";;
#load "groebnerBasis.cmo";;
#load "nc_gasbi.cmo";;

open GroebnerBasis

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

let x = 'x' and y = 'y' and z = 'z';;
let vars = [x;y;z];;

let mp = [1;1;0];; (* only z is fully known, and only g^x and g^y are known *)

let m1 = [(Num.Int 1,[1;0;0])];; (* x *)
let m2 = [(Num.Int 1,[0;1;0])];; (* y *)
let m3 = [(Num.Int 1,[0;0;1])];; (* z *)
let m4 = [(Num.Int 1,[1;1;0])];; (* xy *)
let m5 = [(Num.Int 1,[0;1;1])];; (* yz *)

let p1 = mpoly_add m4 m2;; (* xy+y *)

let gb = groebner vars mp ([p1;m4]);;
is_deduc vars mp gb m5;;



open Nc_gasbi;;

(* Exemples *)
let m1 = {coeff=Num.Int 1; vars=[27]; size=(2,2); length=1};;
let m2 = {coeff=Num.Int 1; vars=[27;78]; size=(2,4); length=2};;
let m3 = {coeff=Num.Int 1; vars=[27;27;78]; size=(2,4); length=3};;
let m4 = {coeff=Num.Int 1; vars=[27;27]; size=(2,2); length=2};;
let m5 = {coeff=Num.Int 1; vars=[78]; size=(2,4); length=1};;

let p1 = mpoly_add [m1] [m2];;
mpoly_mul [m1] [m2;m1];;

let base = DBase.from_list [[m1];[m2];[m2;m1];[m4];[m5]];;
DBase.get_all_prefix_lt base [1;2] ;;

get_all_products [1;2]   (DBase.from_list [[m1];[m2];[m2;m1];[m4];[m5]]);;

let base2 =  DBase.from_list [[m3];[m5]];;

monom_critical_pairs [1;1] base;;
monom_critical_pairs [1;1] base2;;

reduce_1 [m3] (DBase.from_list [[m2;m4];[m5;m3]]);;
reduce [m3;m1] (DBase.from_list [[m4;m1];[m5];[m2;m1];[m1]]);;


let lb =  [[m2];[m2;m1]];;
get_all_products m1.vars (DBase.from_list lb);;
reduce_1 [m1] (DBase.from_list lb);;
deduce [m1] lb;;
deduce [m2] lb;;
deduce [m3] lb;;
deduce [m4] lb;;
deduce [m5] lb;;
inverter [m1] lb;;
inverter [m2] lb;;
inverter [m3] lb;;
inverter [m4] lb;;
inverter [m5] lb;;
