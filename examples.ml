(* Grobner basis computations for K[X]-module *)
#use "topfind";;
#require "num";;
#cd "_build/src/";;
#load "utils.cmo";;
#load "groebnerBasis.cmo";;

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
