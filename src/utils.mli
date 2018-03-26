val itlist : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b 
val lexord : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val lexord_lt : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val tryfind : ('a -> 'b) -> 'a list -> 'b 
val distinctpairs : 'a list -> ('a * 'a) list
