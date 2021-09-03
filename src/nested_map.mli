open Names

type 'a t

val create : 'a -> 'a t
val current : 'a t -> 'a
val map : 'a t -> f:('a -> 'a) -> 'a t
val with_module : 'a t -> Module_name.t -> f:('a t option -> 'a t) -> 'a t
val remove_module : 'a t -> Module_name.t -> 'a t

val fold_modules
  :  'a t
  -> init:'acc
  -> f:(key:Module_name.t -> data:'a t -> 'acc -> 'acc)
  -> 'acc
