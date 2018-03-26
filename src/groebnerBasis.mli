(* Grobner basis computations for K[X]-module *)

type mon = Num.num * int list
type pol = mon list
type i_var_set = int list

val mpoly_add : pol -> pol -> pol 

val groebner : 'a -> i_var_set -> mon list list -> mon list list 
val is_deduc : 'a -> i_var_set -> mon list list -> pol -> bool 
 
