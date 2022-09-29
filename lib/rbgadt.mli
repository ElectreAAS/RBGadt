type 'a t

val empty : 'a t
val mem : 'a -> 'a t -> bool
val add : 'a -> 'a t -> 'a t